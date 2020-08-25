{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Config
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module Config where

import qualified Data.Text as Text
import           Helpers

data Config = Config
    { url             :: Text.Text,
      title           :: Text.Text,
      description     :: Text.Text,
      postsOnHome     :: Int,
      baseUrl         :: Text.Text,
      extension       :: Text.Text,
      homeIsPage      :: Bool,
      convertCmd      :: Text.Text,
      showCategories  :: Bool,
      i8nCategory     :: Text.Text,
      i8nCategories   :: Text.Text,
      i8nArchive      :: Text.Text,
      i8nPosts        :: Text.Text,
      i8nFiledUnder   :: Text.Text,
      i8nReadMore     :: Text.Text
    }

-------------------------------------------------------------------------------
-- | 'parseConfig' parses the config file, without parser combinators :-)
parseConfig :: Text.Text -> [(Text.Text, Text.Text)]
parseConfig config = map cleanUpValues $ map (Text.breakOn "=")
                       $ filter filterConfig $ map Text.strip
                       $ Text.lines config
    where
        filterConfig t = not $ Text.null t || Text.head t == '#'
        cleanUpValues (f, s) = (Text.strip f, Text.strip $ Text.tail s)

-------------------------------------------------------------------------------
-- | 'getConfig' parses the config file contents and returns a Config record
getConfig :: Text.Text -> Config
getConfig rawConfig = Config
    { url = justOrError (lookup "url" config)
          "Could not find 'url' in your config"
    , title = justOrError (lookup "title" config)
          "Could not find 'title' in your config"
    , description = justOrError (lookup "description" config)
          "Could not find 'description' in your config"
    , postsOnHome = Helpers.textToInt
          $ justOrError (lookup "posts_on_home" config)
          "Could not find 'posts_on_home' in your config"
    , baseUrl = justOrError (lookup "url" config)
          "Could not find 'url' in your config"
    , extension = justOrDefault (lookup "extension" config) ".md"
    , homeIsPage = Helpers.textToBool
          $ justOrDefault (lookup "home_is_page" config) "1"
    , convertCmd = justOrError (lookup "convert" config)
          "Could not find 'convert' in your config"
    , showCategories = Helpers.textToBool
          $ justOrDefault (lookup "show_categories" config) "1"
    , i8nCategory = justOrDefault (lookup "i8n_category" config) "Category:"
    , i8nCategories = justOrDefault (lookup "i8n_categories" config)
        "Categories"
    , i8nArchive = justOrDefault (lookup "i8n_archive" config) "Archive"
    , i8nPosts = justOrDefault (lookup "i8n_posts" config) "Posts"
    , i8nFiledUnder = justOrDefault (lookup "i8n_filed_under" config) 
        "Filed under:"
    , i8nReadMore = justOrDefault (lookup "i8n_readmore" config) "Read more..."
    }
    where
        config = parseConfig rawConfig
