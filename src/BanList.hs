module BanList (BanList, loadBanList) where

import Control.Applicative hiding (optional, many, (<|>))
import Reddit.API.Types.User
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.Text as Text

type BanList = [Username]

loadBanList :: FilePath -> IO (Either ParseError BanList)
loadBanList = parseFromFile banList

banList :: Parser BanList
banList = many (username <* spaces) <?> "banList"

username :: Parser Username
username = (do
  u <- Username . Text.pack <$> manyTill usernameChars space
  optional comment
  return u) <?> "username"

comment :: Parser ()
comment = (do
  _ <- string "--"
  _ <- manyTill anyChar (try $ char '\n')
  return ()) <?> "comment"

usernameChars :: Parser Char
usernameChars =
  alphaNum
  <|> char '-'
  <|> char '_'
