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
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Paths_intolerable_bot
import Prelude hiding (log)
import Reddit.API
import Reddit.API.Types.Comment
import Reddit.API.Types.Listing
import Reddit.API.Types.Post
import Reddit.API.Types.Subreddit
import Reddit.API.Types.User
import Reddit.Bot (wait)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Reddit.API.Types.Comment as Comment

type M a = RedditT (StateT (Set (Either PostID CommentID)) (ReaderT (Username, SubredditName, Text, FilePath) IO)) a

instance Ord PostID where
  compare (PostID a) (PostID b) = compare a b

instance Ord CommentID where
  compare (CommentID a) (CommentID b) = compare a b

main :: IO ()
main = do
  args <- fmap (fmap Text.pack) getArgs
  case args of
    user:pass:sub:fn:[] -> do
      putStrLn "Starting..."
      runBot (Username user) pass (R sub) (Text.unpack fn)
    _ -> do
      putStrLn "Usage: intolerable-bot USERNAME PASSWORD SUBREDDIT"
      exitFailure

runBot :: Username -> Text -> SubredditName -> FilePath -> IO ()
runBot user pass sub filename = do
  file <- liftIO $ Text.readFile =<< getDataFileName "reply.md"
  err <- runReaderT (runStateT (runRedditWithRateLimiting username pass (checkPrevious >> act)) Set.empty) (user, sub, file, filename)
  logIO filename err
  print err
  threadDelay (60 * 1000 * 1000)
  runBot user pass sub filename
  where Username username = user

act :: M ()
act = forever $ do
  (user, subreddit, _, _) <- lift $ lift ask
  Listing _ _ comments <- getNewSubredditComments subreddit
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
  (_, _, _, file) <- lift $ lift ask
  liftIO $ logIO file a

logIO :: Show a => FilePath -> a -> IO ()
logIO fp a = do
  time <- formatTime defaultTimeLocale rfc822DateFormat <$> getCurrentTime
  let outputString = time <> ": " <> show a <> "\n"
  putStr outputString
  appendFile fp outputString


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
