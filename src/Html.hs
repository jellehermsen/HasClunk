{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Html
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module Html (categoryIndex, archive, index, postHtml, pageHtml) where

import qualified Data.Text        as Text
import qualified Config           as Config
import           PostMeta
import           Helpers


-------------------------------------------------------------------------------
-- | 'categoryIndex' generates the Html for a category page
categoryIndex :: Config.Config -> (Text.Text, [PostMeta]) -> Text.Text
categoryIndex config (cat, posts) = Text.concat
    ["<article class=\"categories\">\n"
     , "<header><h1>Category: ", cat, "</h1></header>\n"
     , "<section><ul>\n", listPosts baseUrl posts, "</ul></section>\n"
     , "</article>"]
    where
        baseUrl = Config.baseUrl config
        i8nCategory = Config.i8nCategory config


-------------------------------------------------------------------------------
-- | 'listPosts' generates a html list of posts (sans <ul>)
listPosts :: Text.Text -> [PostMeta] -> Text.Text
listPosts baseUrl = Text.concat . map (listPost baseUrl)


-------------------------------------------------------------------------------
-- | 'listPost' creates a li with a link for a post
listPost :: Text.Text -> PostMeta -> Text.Text
listPost baseUrl (PostMeta t d f _ _ _) = Text.concat
    ["<li>"
    , "<a href=\"", baseUrl, "posts/", htmlExt f, "\">"
    ,t," <time>(", d, ")</time></a>"
    , "</li>\n"]


-------------------------------------------------------------------------------
-- | 'archive' generates the Html for the archive page
archive :: Config.Config -> [(Text.Text, [PostMeta])] -> [PostMeta] -> Text.Text
archive config cats posts = Text.concat
    ["<article class=\"archive\">\n"
    , "<header><h1>", i8nArchive, "</h1></header>\n"
    , categories
    , "<h2>", i8nPosts, "</h2>\n"
    , "<ul>\n", listPosts baseUrl posts, "</ul>\n"
    , "</article>"]
    where
        baseUrl = Config.baseUrl config
        i8nArchive = Config.i8nArchive config
        i8nPosts = Config.i8nPosts config
        i8nCategories = Config.i8nCategories config
        categories = if Config.showCategories config
            then
                Text.concat ["<section><h2>", i8nCategories, "</h2>\n"
                    , "<ul>\n", listCategories baseUrl cats , "</ul>\n"
                ]
            else
                ""


-------------------------------------------------------------------------------
-- | 'categoryList' generates a html list of categories (sans </ul>)
listCategories :: Text.Text -> [(Text.Text, [PostMeta])] -> Text.Text
listCategories baseUrl cats = Text.concat $ map (\x -> Text.concat
    ["<li><a href=\"", baseUrl, "categories/", (fst x)
    , ".html\">", (fst x), "</a></li>\n"])
    cats


-------------------------------------------------------------------------------
-- | 'index' gives all posts for display in the index.html
index :: Config.Config -> [PostMeta] -> Text.Text
index config posts = Text.concat $ map (postHtml config False True) posts


-------------------------------------------------------------------------------
pageHtml :: Text.Text -> Text.Text -> Text.Text -> Text.Text
pageHtml content title filename =
    Text.concat
      ["<article>"
      , "<header>\n" , pageHeader title filename, "</header>\n"
      , "<section>\n", content,         "</section>\n"
      , "</article>\n\n"]


-------------------------------------------------------------------------------
-- | 'pageHeader' gives the header for a specific post
pageHeader :: Text.Text -> Text.Text -> Text.Text
pageHeader title filename = Text.concat
    ["<h1>", title , "</h1>\n"]


-------------------------------------------------------------------------------
-- | 'postHtml' adds header/footer..etc to a given post, you can specifiy
--   whether to display the entire post, or only an excerpt/teaser with a
--   "read more" link
postHtml ::  Config.Config -> Bool -> Bool -> PostMeta -> Text.Text
postHtml config topLevel excerpt post = if excerpt
  && Text.isInfixOf "<!--more-->" content
    then
        Text.concat
          ["<article>"
          , "<header>\n" , postHeader baseUrl topLevel post,   "</header>\n"
          , "<section>\n", untilMore content, moreLink post, "</section>\n"
          , "<footer>\n" , footer, "</footer>"
          , "</article>\n\n"]
    else Text.concat
          ["<article>"
          , "<header>\n" , postHeader baseUrl topLevel post, "</header>\n"
          , "<section>\n", content,         "</section>\n"
          , "<footer>\n" , footer, "</footer>\n"
          , "</article>\n\n"]
    where
        baseUrl = Config.baseUrl config
        showCategories = Config.showCategories config
        content = Text.replace "{base_url}" baseUrl $ html post
        untilMore = fst . Text.breakOn "<!--more-->"
        i8nReadMore = Config.i8nReadMore config
        moreLink p = Text.concat ["<a class=\"readmore\" href=\"", baseUrl
          ,"posts/" ,htmlExt $ fileName p, "\">", i8nReadMore, "</a>"]
        footer = if showCategories
            then
              Text.concat [
                "<footer>\n", postFooter config post, "</footer>\n"
              ]
            else
              ""



-------------------------------------------------------------------------------
-- | 'postHeader' gives the header for a specific post
postHeader :: Text.Text -> Bool -> PostMeta -> Text.Text
postHeader baseUrl topLevel post = Text.concat
    ["<", tag topLevel, "><a href=\"", link, "\">", title post , "</a></"
    , tag topLevel, ">\n" , "<time>" , date post, "</time>\n"]
    where
        link = Text.concat [baseUrl, "posts/", htmlExt $ fileName post]
        tag True = "h1"
        tag False = "h2"


-------------------------------------------------------------------------------
-- | 'postFooter' gives the footer for a specific post
postFooter :: Config.Config -> PostMeta -> Text.Text
postFooter config post = Text.concat
  ["<span class=\"filed\">", i8nFiledUnder, "</span><ul>\n"
  , categoryLinks baseUrl post, "</ul>\n"]
  where
    baseUrl = Config.baseUrl config
    i8nFiledUnder = Config.i8nFiledUnder config


-------------------------------------------------------------------------------
-- | 'categoryLinks' creates a text with all the li's for the categories
--   that are linked to a post
categoryLinks :: Text.Text -> PostMeta -> Text.Text
categoryLinks baseUrl post = Text.concat
  $ map (categoryLink baseUrl) $ categories post


-------------------------------------------------------------------------------
-- | 'categoryLink' creates a link to a category
categoryLink :: Text.Text -> Text.Text -> Text.Text
categoryLink baseUrl cat = Text.concat
  ["<li><a href=\"", baseUrl, "categories/", cat
  , ".html\">", cat, "</a></li>\n"]
