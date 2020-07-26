module WebApi.Recipe.Data (
  Recipe(..),
  TagsRequest(..)
) where

import Data.Text
import Data.Aeson
import GHC.Generics


data Recipe = MkRecipe
  { name :: Text
  , authorId :: Int
  , text :: Text
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON)


newtype TagsRequest = TagsRequest { tags :: [Int] }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)