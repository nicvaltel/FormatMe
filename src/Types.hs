module Types where

import Data.Set (Set)

type UserIdx = Int
newtype Model = Model {users :: Set UserIdx}

