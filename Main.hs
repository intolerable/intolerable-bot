module Main where

import BanList

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Options.Applicative
import Prelude hiding (log)
import Reddit hiding (bans)
import Reddit.Types.Comment
import Reddit.Types.Listing
import Reddit.Types.Post
import Reddit.Types.Subreddit
import Reddit.Types.User
import System.Exit (exitFailure)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Reddit.Types.Comment as Comment
import qualified Reddit.Types.Options as Reddit

type M a = RedditT (StateT (Set (Either PostID CommentID)) (ReaderT (Username, SubredditName, Text, Maybe FilePath, [Username]) IO)) a

data Options =
  Options { username :: Username
          , password :: Text
          , subreddit :: SubredditName
          , replyFileName :: FilePath
          , logFileName :: Maybe FilePath
          , banListFileName :: Maybe FilePath }
  deriving (Show, Read, Eq)

opts :: Parser Options
opts = Options <$> (Username <$> argument text (metavar "USERNAME"))
               <*> argument text (metavar "PASSWORD")
               <*> (R <$> argument text (metavar "SUBREDDIT"))
               <*> argument str (metavar "REPLY_FILE")
               <*> optional (strOption (long "log-file" <> metavar "LOG"))
               <*> optional (strOption (long "ban-list-file" <> metavar "BAN_LIST"))
  where text = fmap Text.pack str

main :: IO ()
main = do
  o <- execParser $ info (helper <*> opts) fullDesc
  putStrLn "Starting..."
  runBot o

runBot :: Options -> IO ()
runBot o@(Options user pass sub replyFile logFile banList) = do
  file <- liftIO $ Text.readFile replyFile
  bl <- case banList of
    Just fn -> do
      list <- loadBanList fn
      case list of
        Left err -> do
          print err
          exitFailure
        Right l -> return l
    Nothing -> return []
  (err, _) <- runReaderT (runStateT (runRedditWithRateLimiting u pass (checkPrevious >> act)) Set.empty) (user, sub, file, logFile, bl)
  logIO logFile err
  threadDelay (60 * 1000 * 1000)
  runBot o
  where Username u = user

act :: M ()
act = forever $ do
  (user, sub, _, _, bans) <- lift $ lift ask
  Listing _ _ comments <- getNewComments' (Reddit.Options Nothing (Just 100)) (Just sub)
  forM_ (filter (shouldRespond user) comments) $ \comment -> do
    if Comment.author comment `elem` bans
      then
        log $ "Banned user trying to post: " <> show (Comment.author comment)
      else do
        alreadyInPosted <- query $ directParent comment
        unless alreadyInPosted $ do
          p <- getPostInfo $ parentLink comment
          case content p of
            Link _ -> return ()
            _ -> do
              case Comment.inReplyTo comment of
                Nothing -> do
                  log (commentID comment, Comment.author comment, Comment.parentLink comment)
                  -- TODO: make sure we didn't already answer
                  handle $ directParent comment
                Just c -> do
                  commentInfo <- getCommentInfo c
                  if Comment.author commentInfo == user
                    then
                      return ()
                    else do
                      log (commentID comment, Comment.author comment, Comment.parentLink comment)
                      -- TODO: make sure we didn't already answer
                      handle $ directParent comment
  wait 15

wait :: Int -> M ()
wait s = liftIO $ threadDelay (s * 1000000)

log :: Show a => a -> M ()
log a = do
  (_, _, _, fn, _) <- lift $ lift ask
  liftIO $ logIO fn a

logIO :: Show a => Maybe FilePath -> a -> IO ()
logIO fp a = do
  time <- formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
  let outputString = time <> ": " <> show a <> "\n"
  putStr outputString
  whenJust fp $ \f -> appendFile f outputString

checkPrevious :: M ()
checkPrevious = do
  (user, _, _, _, _) <- lift $ lift ask
  Listing _ _ previousComments <- getUserComments user
  mapM_ (record . directParent) previousComments

expandComment :: PostID -> CommentReference -> Reddit [CommentReference]
expandComment _ (Actual c) = return [Actual c]
expandComment p (Reference _ c) = getMoreChildren p c

handle :: Either PostID CommentID -> M ()
handle t = do
  (_, _, file, _, _) <- lift $ lift ask
  record t
  res <- nest $ reply' t file
  case res of
    Left x -> log x
    Right _ -> return ()
  where reply' = either reply reply

shouldRespond :: Username -> Comment -> Bool
shouldRespond (Username user) c =
  ("/u/" <> Text.toLower user) `Text.isInfixOf` Text.toLower (body c) &&
  Comment.author c /= Username user

alreadyAnswered :: Username -> [Comment] -> Bool
alreadyAnswered user = any ((== user) . Comment.author)

reset :: M ()
reset = lift $ put mempty

record :: Either PostID CommentID -> M ()
record = lift . modify . Set.insert

query :: Either PostID CommentID -> M Bool
query a = lift $ liftM (Set.member a) get

directParent :: Comment -> Either PostID CommentID
directParent c =
  case Comment.inReplyTo c of
    Just x -> Right x
    Nothing -> Left $ Comment.parentLink c

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()
