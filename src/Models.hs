module Models (
  Person(..)
) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)


data Person = Person
  { name :: Text
  , age :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)