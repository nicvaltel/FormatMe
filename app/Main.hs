{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Text                        as T
import           Telegram.Bot.API (Token(..))
import Configuration.Dotenv (parseFile)
import Data.Either.Combinators(maybeToRight)
import qualified Bot

newtype BotConfig = BotConfig
  { botToken :: String
  }

envFile :: FilePath
envFile = "app-config.env"

main :: IO ()
main = do
  cfg <- getCfg <$> parseFile envFile
  case cfg of
    Left err -> error err
    Right BotConfig{botToken} -> do
      Bot.runBot (Token . T.pack $ botToken)
  where
    getCfg :: [(String, String)] -> Either String BotConfig
    getCfg env = do
      botToken <- maybeToRight "No TOKEN defined" (lookup "BOT_TOKEN" env)
      pure BotConfig {botToken}
