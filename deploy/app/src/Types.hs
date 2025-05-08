module Types where

import Data.Set (Set)
import Data.Text (Text)
import Telegram.Bot.API

type UserIdx = Int

newtype Model = Model {users :: Set UserIdx}

data Action
  = InlineEcho InlineQueryId Text
  | StickerEcho InputFile ChatId
  | Echo Text (Maybe UserIdx)
