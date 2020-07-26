module WebApi.Recipe.RecipeApi (
  RecipeAPI,
  server
) where

import Servant
import WebApi.Recipe.Data
import qualified WebApi.Recipe.RecipeDb as Db
import WebApi.MonadDb

type RecipeAPI =
       "favorite_recipes" :> Get '[JSON] [Recipe]
  :<|> "recipe_by_id" :> Capture "id" Int :> Get '[JSON] Recipe
  :<|> "trending_recipes" :> Get '[JSON] [Recipe]
  :<|> "recipes_by_tags" :> ReqBody '[JSON] TagsRequest :> Get '[JSON] Recipe


favoriteRecipes :: (MonadDb m) => m [Recipe]
favoriteRecipes = undefined

recipeById :: (MonadDb m) => Int -> m Recipe
recipeById id = do
  res <- Db.test
  pure $ MkRecipe "name" res "text"

trendingRecipes :: (MonadDb m) => m [Recipe]
trendingRecipes = undefined

recipesByTags :: (MonadDb m) => TagsRequest -> m Recipe
recipesByTags = undefined

server :: MonadDb m => ServerT RecipeAPI m
server = favoriteRecipes :<|> recipeById :<|> trendingRecipes :<|> recipesByTags


