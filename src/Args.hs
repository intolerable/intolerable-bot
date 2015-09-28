module Args
  ( optionsFromArgs
  , resolve
  , Config(..)
  , Options(..)
  , Settings(Settings)
  , Reply(..)
  , Bans(..)
  , Password
  , ReplyText
  , RefreshTime ) where

import Control.Applicative
import Data.Foldable
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Reddit.Types.Subreddit
import Reddit.Types.User
import Options.Applicative
import qualified Data.Map as Map

type Password = Text
type ReplyText = Text
type RefreshTime = Int

data Options = ConfigFile FilePath
  deriving (Show, Read, Eq)

optionsFromArgs :: IO Options
optionsFromArgs =
  execParser $ info (helper <*> options) description
    where
      description = mconcat
        [ fullDesc
        , header "intolerable-bot - Help new players on Reddit" ]

options :: Parser Options
options = ConfigFile <$> argument str (metavar "CONFIG" <> help "Config file to use")

data Config =
  Config { defaultConfig :: Settings
         , subredditConfigs :: Map SubredditName Settings }
  deriving (Show, Read, Eq)

instance FromJSON Config where
  parseJSON (Object o) =
    Config <$> parseJSON (Object o)
           <*> (Map.mapKeys R <$> o .: "subreddits")
  parseJSON _ = mempty

data Settings =
  Settings { _username :: Maybe Username
           , _password :: Maybe Password
           , _reply :: Maybe Reply
           , _bans :: [Bans]
           , _refreshTime :: Maybe RefreshTime
           , _classifier :: Maybe FilePath
           , _useClassifier :: Bool
           , _verbose :: Maybe Bool }
  deriving (Show, Read, Eq)

instance Monoid Settings where
  mempty = Settings Nothing Nothing Nothing [] Nothing Nothing False Nothing
  Settings u1 p1 r1 b1 t1 c1 x1 v1 `mappend` Settings u2 p2 r2 b2 t2 c2 x2 v2 =
    Settings (u1 <|> u2) (p1 <|> p2) (r1 <|> r2) (b1 <> b2) (t1 <|> t2) (c1 <|> c2) (x1 || x2) (v1 <|> v2)

resolve :: Settings -> Map a Settings -> Map a Settings
resolve x = fmap (x <>)

instance FromJSON Settings where
  parseJSON (Object o) =
    Settings <$> (o .:? "username" <|> o .:? "user")
             <*> (o .:? "password" <|> o .:? "pass")
             <*> asum [ Just . ReplyLiteral <$> o .: "reply"
                      , Just . ReplyFilePath <$> o .: "reply_file"
                      , pure Nothing ]
             <*> parsedBans
             <*> asum [ o .:? "refresh"
                      , o .:? "refresh_time"
                      , o .:? "refresh_interval" ]
             <*> o .:? "classifier_file"
             <*> (o .: "use_classifier" <|> pure False)
             <*> o .:? "verbose"
    where
      parsedBans =
        mappend <$> (pure . BansList . map Username <$> (o .: "bans" <|> pure []))
                <*> ((pure . BansFilePath <$> o .: "bans_file") <|> pure [])
  parseJSON Null = pure mempty
  parseJSON _ = mempty

data Bans = BansList [Username]
          | BansFilePath FilePath
  deriving (Show, Read, Eq)

data Reply = ReplyLiteral ReplyText
           | ReplyFilePath FilePath
  deriving (Show, Read, Eq)

