{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bot where

import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Maybe

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText, updateMessageSticker)
import           Telegram.Bot.API.InlineMode.InlineQueryResult
import           Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import Configuration.Dotenv (parseFile)
import Data.Either.Combinators(maybeToRight)
import Control.Monad.IO.Class (liftIO)
import Shower (showerString)

type Model = ()

data Action
  = InlineEcho InlineQueryId Text
  | StickerEcho InputFile ChatId
  | Echo Text

echoBot :: BotApp Model Action
echoBot = BotApp
  { botInitialModel = ()
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _
  | isJust $ updateInlineQuery update =  do
      query <- updateInlineQuery update
      let queryId = inlineQueryId query
      let msg =  inlineQueryQuery query
      Just $ InlineEcho queryId msg
  | isJust $ updateMessageSticker update = do
    fileId <- stickerFileId <$> updateMessageSticker update
    chatOfUser <- updateChatId update
    pure $ StickerEcho (InputFileId fileId) chatOfUser
  | otherwise = case updateMessageText update of
      Just text -> Just (Echo text)
      Nothing   -> Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  InlineEcho queryId msg -> model <# do
    let result = (defInlineQueryResultGeneric (InlineQueryResultId msg))
          { inlineQueryResultTitle = Just msg
          , inlineQueryResultInputMessageContent = Just (defaultInputTextMessageContent msg)
          }
        thumbnail = defInlineQueryResultGenericThumbnail result
        article = defInlineQueryResultArticle thumbnail
        answerInlineQueryRequest = defAnswerInlineQuery queryId [article]
    _ <- runTG  answerInlineQueryRequest
    pure ()
  StickerEcho file chat -> model <# do
    _ <- runTG
        (defSendSticker (SomeChatId chat) file)
    pure ()
  Echo msg -> model <# do
    liftIO $ putStrLn $ "RECIEVE: " <> T.unpack msg
    case showerString (T.unpack msg) of
      Left err -> pure (T.pack err)
      Right result -> pure (T.pack result)
    -- pure $ "REPLY: " <> msg -- or replyText msg

runBot :: Token -> IO ()
runBot token = do
  env <- defaultTelegramClientEnv token
  startBot_ echoBot env
