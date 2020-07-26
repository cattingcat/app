module WebApi.People.Data (
  Person(..)
) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)


data Person = MkPerson
  { name :: Text
  , age :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)