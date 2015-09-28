module Bot where

import Args
import Control.Applicative.Trans.Either
import Control.Concurrent.WriteSem

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (catchJust)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Binary
import Data.Char
import Data.Classifier.NaiveBayes (NaiveBayes)
import Data.Coerce
import Data.Default.Class
import Data.Function (fix)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format
import Data.Yaml
import Reddit hiding (failWith, bans)
import Reddit.Types.Comment (PostComments(..), CommentReference(..))
import Reddit.Types.Listing
import Reddit.Types.Subreddit (SubredditName(..))
import Reddit.Types.User (Username(..))
import System.Exit
import System.IO
import System.IO.Error
import qualified Data.Bounded.Set as Bounded
import qualified Data.Classifier.NaiveBayes as NB
import qualified Data.Counter as Counter
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Reddit.Types.Comment as Comment
import qualified Reddit.Types.Post as Post

data ConcreteSettings =
  ConcreteSettings { username :: Username
                   , password :: Password
                   , subreddit :: SubredditName
                   , replyText :: ReplyText
                   , refreshTime :: RefreshTime
                   , bans :: [Username]
                   , classifier :: Maybe (NaiveBayes Bool Text)
                   , useClassifier :: Bool
                   , verboseOutput :: Bool }
  deriving (Show)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  ConfigFile fp <- optionsFromArgs
  decodeFileEither fp >>= \case
    Left err -> do
      print err
      exitFailure
    Right (Config b m) -> do
      resolvedSettings <- mapM confirm $ resolve b m
      let (lefts, rights) = Map.mapEither id resolvedSettings
      if Map.null lefts
        then do
          sem <- newWriteSemIO
          void $ mapConcurrently (\(k, s) -> run sem (s k)) $ Map.toList rights
        else do
          Map.foldrWithKey handleError (return ()) lefts
          exitFailure

handleError :: SubredditName -> [Text] -> IO () -> IO ()
handleError (R r) errs m = m >> do
  Text.putStrLn $ pluralize errs "Error" <> " with settings for subreddit /r/" <> r <> ":"
  forM_ errs $ \err ->
    Text.putStr $ "  - " <> err

pluralize :: [a] -> Text -> Text
pluralize [_] x = x
pluralize _ x = x <> "s"

confirm :: Settings -> IO (Either [Text] (SubredditName -> ConcreteSettings))
confirm (Settings u p r b t c x v) =
  runEitherA $
    subredditLastSettings
      <$> justOr ["Missing username"] u
      <*> justOr ["Missing password"] p
      <*> loadReply r
      <*> pure (fromMaybe 5 t)
      <*> loadBans b
      <*> sequenceA (fmap loadClassifier c)
      <*> pure x
      <*> pure (fromMaybe False v)

subredditLastSettings :: Username -> Password -> ReplyText -> RefreshTime -> [Username] -> Maybe (NaiveBayes Bool Text) -> Bool -> Bool -> SubredditName -> ConcreteSettings
subredditLastSettings u p r t b n x v s = ConcreteSettings u p s r t b n x v

loadBans :: [Bans] -> EitherA [Text] IO [Username]
loadBans = fmap concat . sequenceA . map f
  where
    f (BansList us) = pure us
    f (BansFilePath fp) = EitherA $
      decodeFileEither fp >>= \case
        Left err -> return $ Left [Text.pack $ show err]
        Right xs -> return $ Right $ map Username xs

loadReply :: Maybe Reply -> EitherA [Text] IO ReplyText
loadReply x = case x of
  Just r -> case r of
    ReplyLiteral lit -> pure lit
    ReplyFilePath fp -> readReplyFile fp
  Nothing -> failWith ["Missing reply"]

readReplyFile :: FilePath -> EitherA [Text] IO ReplyText
readReplyFile fp = EitherA $ catchJust f (Right <$> Text.readFile fp) (return . Left . return)
  where
    f (isDoesNotExistError -> True) = Just "Reply file does not exist"
    f (isPermissionError -> True) = Just "Incorrect permissions for reply file"
    f _ = Nothing

loadClassifier :: FilePath -> EitherA [Text] IO (NaiveBayes Bool Text)
loadClassifier fp = EitherA $ f <$> decodeFileOrFail fp
  where
    f (Left _) = Left ["Classifier could not be read"]
    f (Right x) = pure x

run :: WriteSem -> ConcreteSettings -> IO ()
run sem settings =
  withAsync (loopWith (forever $ commentsLoop sem) sem settings) $ \c ->
    case classifier settings of
      Just _ ->
        withAsync (loopWith (forever $ postsLoop sem) sem settings) $ \p ->
          void $ waitBoth c p
      Nothing -> wait c

loopWith :: RedditT (ReaderT ConcreteSettings IO) () -> WriteSem -> ConcreteSettings -> IO ()
loopWith act sem settings = do
  res <- flip runReaderT settings $
    runResumeRedditWith def { customUserAgent = Just "intolerable-bot v0.1.0.0"
                            , loginMethod = Credentials (coerce (username settings)) (password settings)
                            , rateLimitingEnabled = False } act
  case res of
    Left (APIError CredentialsError, _) ->
      withWriteSem sem $
        Text.putStrLn $ "Username / password details incorrect for /r/" <> coerce (subreddit settings)
    Left (err, Nothing) -> do
      liftIO $ print err
      (5 * refreshTime settings) `seconds` threadDelay
      loopWith act sem settings
    Left (err, Just resume) -> do
      liftIO $ print err
      loopWith resume sem settings
    Right () -> return ()

postsLoop :: WriteSem -> RedditT (ReaderT ConcreteSettings IO) ()
postsLoop sem = do
  u <- lift $ asks username
  r <- lift $ asks subreddit
  t <- lift $ asks refreshTime
  rt <- lift $ asks replyText
  cls <- lift $ asks (fromJust . classifier)
  use <- lift $ asks useClassifier
  withInitial (Bounded.empty 500) $ \loop set -> do
    Listing _ _ ps <- getPosts' (Options Nothing (Just 100)) New (Just r)
    writeLogEntry sem r "got listing"
    let news = filter (\x -> not $ Bounded.member (Post.postID x) set) ps
    forM_ news $ \p ->
      unless (Post.author p == u) $
        case Post.content p of
          Post.SelfPost m _ -> do
            let c = Counter.fromList $ process m
            case NB.test cls c of
              Just True ->
                if use
                  then do
                    PostComments _ cs <- getPostComments $ Post.postID p
                    actuals <- resolveComments (Post.postID p) cs
                    unless (any ((== u) . Comment.author) actuals) $ do
                      botReply <- reply p rt
                      writeLogEntry sem r $ mconcat
                        [ "Auto-responded to "
                        , coerce $ Post.postID p
                        , " ("
                        , coerce botReply
                        , ")" ]
                  else
                    writeLogEntry sem r $ mconcat
                      [ "Possible AI match @ "
                      , coerce $ Post.postID p ]
              _ -> return ()
          _ -> return ()
    unless (null news) $ writeLogEntry sem r "got listing"
    t `seconds` threadDelay
    loop $ Bounded.insertAll (Post.postID <$> news) set

commentsLoop :: WriteSem -> RedditT (ReaderT ConcreteSettings IO) ()
commentsLoop sem = do
  r <- lift $ asks subreddit
  t <- lift $ asks refreshTime
  withInitial (Bounded.empty 500) $ \loop set -> do
    Listing _ _ cs <- getNewComments' (Options Nothing (Just 100)) (Just r)
    let news = filter (\x -> not $ Bounded.member (Comment.commentID x) set) cs
    mapM_ (commentResponder sem) news
    unless (null news) $ writeLogEntry sem r "dealt with new comments"
    t `seconds` threadDelay
    loop $ Bounded.insertAll (Comment.commentID <$> news) set

commentResponder :: WriteSem -> Comment -> RedditT (ReaderT ConcreteSettings IO) ()
commentResponder sem c = do
  u <- lift $ asks username
  r <- lift $ asks subreddit
  rt <- lift $ asks replyText
  bs <- lift $ asks bans
  when (shouldRespond u (Comment.body c)) $
    unless (Comment.author c `elem` bs) $ do
      writeLogEntry sem r "found a comment"
      (selfpost, sibs) <- getSiblingComments c
      unless (any ((== u) . Comment.author) sibs) $ do
        writeLogEntry sem r $ "found a comment we didn't already respond to: " <> coerce (Comment.commentID c)
        case Comment.inReplyTo c of
          Just parentComment -> reply parentComment rt >>= logReply r
          Nothing ->
            when selfpost $
              reply (Comment.parentLink c) rt >>= logReply r
  where
    logReply r botReply = writeLogEntry sem r $ mconcat
      [ "Responded to "
      , coerce (Comment.commentID c)
      , " by "
      , coerce (Comment.author c)
      , " ("
      , coerce botReply
      , ")" ]

getSiblingComments :: MonadIO m => Comment -> RedditT m (Bool, [Comment])
getSiblingComments c = do
  let parent = Comment.parentLink c
  PostComments p cs <-
    case Comment.inReplyTo c of
      Just parentComment ->
        getPostSubComments parent parentComment >>= \case
          PostComments p (com:_) -> do
            Listing _ _ cs <- mconcat <$> map Comment.replies <$> resolveComments parent [com]
            return $ PostComments p cs
          x -> return x
      Nothing -> getPostComments parent
  case Post.content p of
    Post.SelfPost _ _ -> (,) True <$> resolveComments parent cs
    _ -> (,) (isJust (Comment.inReplyTo c)) <$> resolveComments parent cs

resolveComments :: MonadIO m => PostID -> [CommentReference] -> RedditT m [Comment]
resolveComments p refs = concat <$> mapM f refs
  where
    f (Actual c) = return [c]
    f (Reference _ cs) = do
      moreComments <- getMoreChildren p cs
      resolveComments p moreComments

shouldRespond :: Username -> Text -> Bool
shouldRespond (Username u) = Text.isInfixOf (Text.toCaseFold $ "u/" <> u) . Text.toCaseFold

withInitial :: a -> ((a -> b) -> a -> b) -> b
withInitial = flip fix

seconds :: MonadIO m => Int -> (Int -> IO ()) -> m ()
n `seconds` f = liftIO $ f $ n * 1000000

writeLogEntry :: MonadIO m => WriteSem -> SubredditName -> Text -> m ()
writeLogEntry sem (R r) t = do
  time <- liftIO getCurrentTime
  let space = " "
  withWriteSem sem $
    mapM_ Text.putStr
      [ makeTime time
      , space
      , "/r/"
      , r
      , ": "
      , t
      , "\n" ]

makeTime :: UTCTime -> Text
makeTime t = Text.pack $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) t

process :: Text -> [Text]
process = filter (not . Text.null) .
          map (Text.map toLower . Text.filter isAlpha) .
          concatMap (Text.splitOn ".") .
          Text.splitOn " " .
          Text.filter (not . (== '-'))
