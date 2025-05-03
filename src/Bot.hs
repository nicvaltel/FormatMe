{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bot where

import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Maybe

import           Telegram.Bot.API
import qualified Telegram.Bot.API as API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText, updateMessageSticker)
-- import           Telegram.Bot.API.InlineMode.InlineQueryResult
-- import           Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import Control.Monad.IO.Class (liftIO)
import Shower (showerString)
import Types
import qualified Data.Set as Set

data Action
  = InlineEcho InlineQueryId Text
  | StickerEcho InputFile ChatId
  | Echo Text (Maybe UserIdx)

echoBot :: FilePath -> Set.Set UserIdx ->  BotApp Model Action
echoBot dbFile users = BotApp
  { botInitialModel = Model users
  , botAction = updateToAction
  , botHandler = handleAction dbFile
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
  | otherwise = do
      let mayUserIdx = extractUserId update
      case updateMessageText update of
        Just text -> Just (Echo text mayUserIdx)
        Nothing   -> Nothing

extractUserId :: Update -> Maybe UserIdx
extractUserId update = do
  msg <- API.updateMessage update
  usr <- API.messageFrom msg
  let API.UserId usrId = API.userId usr
  let userIdx = fromIntegral usrId :: Int
  -- let API.MessageId msgId = API.messageMessageId msg
  -- let msgIdInt = fromIntegral msgId :: Int
  -- let usrName = API.userUsername usr
  -- let parser = RecordMsg usrIdInt usrName msgIdInt <$> plainText
  pure userIdx

handleAction :: FilePath -> Action -> Model -> Eff Action Model
handleAction dbFile action model = case action of
  InlineEcho queryId msg -> model <# do
    -- let result = (defInlineQueryResultGeneric (InlineQueryResultId msg))
    --       { inlineQueryResultTitle = Just msg
    --       , inlineQueryResultInputMessageContent = Just (defaultInputTextMessageContent msg)
    --       }
    --     thumbnail = defInlineQueryResultGenericThumbnail result
    --     article = defInlineQueryResultArticle thumbnail
    --     answerInlineQueryRequest = defAnswerInlineQuery queryId [article]
    -- _ <- runTG  answerInlineQueryRequest
    pure ()
  StickerEcho file chat -> model <# do
    -- _ <- runTG
    --     (defSendSticker (SomeChatId chat) file)
    pure ()
  Echo msg mayUserIdx -> 
    case mayUserIdx of
      Nothing -> model <# replyFormatted msg
      Just userIdx -> 
        if userIdx `Set.member` users model
          then model <# replyFormatted msg
        else Model{users = userIdx `Set.insert` users model} <# do
          liftIO $ appendFile dbFile (show userIdx <> "\n")
          ans <- replyFormatted msg
          replyText ans
          replyText greetings1
          replyText greetings2
          replyText greetings3
  where
    replyFormatted msg = case showerString (T.unpack msg) of
      Left err -> pure (T.pack err)
      Right result -> pure (T.pack result)


greetings1 :: Text
greetings1 = "Привет! Я форматирую структурированный текст со множеством вложений, привожу его к удобному для чтению виду."

greetings2 :: Text
greetings2 = "Попробуй, отправь мне этот текст для примера:"

greetings3 :: Text
greetings3 = "Resource {resourceValue=User {userId=83261fc4-e131-483e-b1a9-2737e9a3aba4, userCreatedAt=2025-05-03 20:10:58.235489 UTC, userName=\"Admin\", userPassScrypt=<omitted>}, resourceHash=ACCB18DB4CC2F85CEDEF654FCCC4A4A8}"


    -- model <# do
    --   -- liftIO $ putStrLn $ "RECIEVE: " <> T.unpack msg
    --   case showerString (T.unpack msg) of
    --     Left err -> pure (T.pack err)
    --     Right result -> pure (T.pack result)
    --   -- pure $ "REPLY: " <> msg -- or replyText msg

runBot :: Token -> FilePath -> IO ()
runBot token dbFile = do
  env <- defaultTelegramClientEnv token
  dbContent <- readFile dbFile
  let users = Set.fromList $ map read (lines dbContent) :: Set.Set UserIdx
  startBot_ (echoBot dbFile users) env
