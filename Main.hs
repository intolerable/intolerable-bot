module Main where

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
import Paths_intolerable_bot
import Prelude hiding (log)
import Reddit.API
import Reddit.API.Types.Comment
import Reddit.API.Types.Listing
import Reddit.API.Types.Post
import Reddit.API.Types.Subreddit
import Reddit.API.Types.User
import Reddit.Bot (wait)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Reddit.API.Types.Comment as Comment

type M a = RedditT (StateT (Set (Either PostID CommentID)) (ReaderT (Username, SubredditName, Text, Maybe FilePath) IO)) a

data Options =
  Options { username :: Username
          , password :: Text
          , subreddit :: SubredditName
          , logFileName :: Maybe FilePath }
  deriving (Show, Read, Eq)

opts :: Parser Options
opts = Options <$> (Username <$> argument text (metavar "USERNAME"))
               <*> argument text (metavar "PASSWORD")
               <*> (R <$> argument text (metavar "SUBREDDIT"))
               <*> optional (strOption (long "log-file" <> metavar "LOG"))
  where text = fmap Text.pack . str

main :: IO ()
main = do
  o <- execParser $ info (helper <*> opts) fullDesc
  putStrLn "Starting..."
  runBot o

runBot :: Options -> IO ()
runBot o@(Options user pass sub filename) = do
  file <- liftIO $ Text.readFile =<< getDataFileName "reply.md"
  (err, _) <- runReaderT (runStateT (runRedditWithRateLimiting u pass (checkPrevious >> act)) Set.empty) (user, sub, file, filename)
  logIO filename err
  threadDelay (60 * 1000 * 1000)
  runBot o
  where Username u = user

act :: M ()
act = forever $ do
  (user, sub, _, _) <- lift $ lift ask
  Listing _ _ comments <- getNewSubredditComments sub
  forM_ (filter (commentMentions user) comments) $ \comment -> do
    alreadyInPosted <- query $ directParent comment
    unless alreadyInPosted $ do
      p <- getPostInfo $ parentLink comment
      case content p of
        Link _ -> return ()
        _ -> do
          log (commentID comment, Comment.author comment, Comment.parentLink comment)
          -- TODO: make sure we didn't already answer
          handle $ directParent comment
  wait 15

log :: Show a => a -> M ()
log a = do
  (_, _, _, fn) <- lift $ lift ask
  liftIO $ logIO fn a

logIO :: Show a => Maybe FilePath -> a -> IO ()
logIO fp a = do
  time <- formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
  let outputString = time <> ": " <> show a <> "\n"
  putStr outputString
  whenJust fp $ \f -> appendFile f outputString

checkPrevious :: M ()
checkPrevious = do
  (user, _, _, _) <- lift $ lift ask
  Listing _ _ previousComments <- getUserComments user
  mapM_ (record . directParent) previousComments

expandComment :: PostID -> CommentReference -> Reddit [CommentReference]
expandComment _ (Actual c) = return [Actual c]
expandComment p (Reference _ c) = getMoreChildren p c

handle :: Either PostID CommentID -> M ()
handle t = do
  (_, _, file, _) <- lift $ lift ask
  record t
  void $ reply' t file
  where reply' = either reply reply

commentMentions :: Username -> Comment -> Bool
commentMentions (Username user) c = ("/u/" <> Text.toLower user) `Text.isInfixOf` Text.toLower (body c)

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
