{-# LANGUAGE OverloadedStrings #-}

module Bot(runBot) where

import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Maybe

import           Telegram.Bot.API
import qualified Telegram.Bot.API as API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText, updateMessageSticker)
import Control.Monad.IO.Class (liftIO)
import Shower (showerString)
import Types
import qualified Data.Set as Set



echoBot :: FilePath -> Set.Set UserIdx ->  BotApp Model Action
echoBot dbFile usersSet = BotApp
  { botInitialModel = Model usersSet
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
  pure userIdx

handleAction :: FilePath -> Action -> Model -> Eff Action Model
handleAction dbFile action model = case action of
  InlineEcho _ _ -> model <#  pure ()
  StickerEcho _ _ -> model <# pure ()
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
      Left err -> pure $ errorMsg <> T.pack err
      Right result -> pure (T.pack result)


greetings1 :: Text
greetings1 = "Hi! I am formatting a structured text with many attachments, bringing it to a form that is easy to read."

greetings2 :: Text
greetings2 = "Try it, send me this text as an example:"

greetings3 :: Text
greetings3 = "Container {containerValue=Product {productId=53211fd2-c131-273c-a1b8-2567a9e3bab2, productCreatedAt=2025-05-03 20:10:58.235489 UTC, productName=\"Battery charger\", productType=<common>}, containerHash=CAAD28AB1CF6A58DECEB465CFFC2B3A7}"

errorMsg :: Text
errorMsg = "Failed to format the text. The text structure is not consistent.\n"

runBot :: Token -> FilePath -> IO ()
runBot token dbFile = do
  env <- defaultTelegramClientEnv token
  dbContent <- readFile dbFile
  let usersSet = Set.fromList $ map read (lines dbContent) :: Set.Set UserIdx
  startBot_ (echoBot dbFile usersSet) env
