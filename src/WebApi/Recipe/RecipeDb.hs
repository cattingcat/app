{-# LANGUAGE QuasiQuotes #-}

module WebApi.Recipe.RecipeDb (
  test,
  favoriteRecipes
) where

import Data.Int
import Data.Text
import Data.Vector
import Hasql.Session
import qualified Hasql.TH as TH

import WebApi.MonadDb


favoriteRecipes :: Session (Vector (Text, Int64))
favoriteRecipes = undefined

test :: MonadDb m => m Int
test = do
  let select = [TH.singletonStatement| select (5::int8) |]

  res <- runSession (statement () select)
  pure $ case res of
    Left _ -> 0
    Right _ -> 1
