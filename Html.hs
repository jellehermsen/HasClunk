{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Html
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen, 2015
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module Html (categoryIndex, archive, index, postHtml, pageHtml) where

import qualified Data.Text        as Text
import           PostMeta
import           Helpers


-------------------------------------------------------------------------------
-- | 'categoryIndex' generates the Html for a category page
categoryIndex :: Text.Text -> (Text.Text, [PostMeta]) -> Text.Text
categoryIndex baseUrl (cat, posts) = Text.concat
    ["<article class=\"categories\">\n"
     , "<header><h1>Category: ", cat, "</h1></header>\n"
     , "<section><ul>\n", listPosts baseUrl posts, "</ul></section>\n"
     , "</article>"]


-------------------------------------------------------------------------------
-- | 'listPosts' generates a html list of posts (sans <ul>)
listPosts :: Text.Text -> [PostMeta] -> Text.Text
listPosts baseUrl = Text.concat . map (listPost baseUrl)


-------------------------------------------------------------------------------
-- | 'listPost' creates a li with a link for a post
listPost :: Text.Text -> PostMeta -> Text.Text
listPost baseUrl (PostMeta t d f _ _) = Text.concat
    ["<li>"
    , "<a href=\"", baseUrl, "posts/", htmlExt f, "\">"
    ,t," <time>(", d, ")</time></a>"
    , "</li>\n"]


-------------------------------------------------------------------------------
-- | 'archive' generates the Html for the archive page
archive :: Text.Text -> [(Text.Text, [PostMeta])] -> [PostMeta] -> Text.Text
archive baseUrl cats posts = Text.concat
    ["<article class=\"archive\">\n"
    , "<header><h1>Archive</h1></header>\n"
    , "<section><h2>Categories</h2>\n"
    , "<ul>\n", listCategories baseUrl cats , "</ul>\n"
    , "<h2>Posts</h2>\n"
    , "<ul>\n", listPosts baseUrl posts, "</ul>\n"
    , "</article>"]


-------------------------------------------------------------------------------
-- | 'categoryList' generates a html list of categories (sans </ul>)
listCategories :: Text.Text -> [(Text.Text, [PostMeta])] -> Text.Text
listCategories baseUrl cats = Text.concat $ map (\x -> Text.concat
    ["<li><a href=\"", baseUrl, "categories/", (fst x)
    , ".html\">", (fst x), "</a></li>\n"])
    cats


-------------------------------------------------------------------------------
-- | 'index' gives all posts for display in the index.html
index :: Text.Text -> [PostMeta] -> Text.Text
index baseUrl posts = Text.concat $ map (postHtml baseUrl True) posts


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
postHtml ::  Text.Text -> Bool -> PostMeta -> Text.Text
postHtml baseUrl excerpt post = if excerpt
  && Text.isInfixOf "<!--more-->" content
    then
        Text.concat
          ["<article>"
          , "<header>\n" , postHeader baseUrl post,   "</header>\n"
          , "<section>\n", untilMore content, moreLink post, "</section>\n"
          , "<footer>\n" , postFooter baseUrl post,   "</footer>"
          , "</article>\n\n"]
    else Text.concat
          ["<article>"
          , "<header>\n" , postHeader baseUrl post, "</header>\n"
          , "<section>\n", content,         "</section>\n"
          , "<footer>\n" , postFooter baseUrl post, "</footer>\n"
          , "</article>\n\n"]
    where
        content = Text.replace "{base_url}" baseUrl $ html post
        untilMore = fst . Text.breakOn "<!--more-->"
        moreLink p = Text.concat ["<a class=\"readmore\" href=\"", baseUrl
          ,"posts/" ,htmlExt $ fileName p, "\">Read more...</a>"]


-------------------------------------------------------------------------------
-- | 'postHeader' gives the header for a specific post
postHeader :: Text.Text -> PostMeta -> Text.Text
postHeader baseUrl post = Text.concat
    ["<h1><a href=\"", link, "\">", title post , "</a></h1>\n"
    , "<time>" , date post, "</time>\n"]
    where
    link = Text.concat [baseUrl, "posts/", htmlExt $ fileName post]


-------------------------------------------------------------------------------
-- | 'postFooter' gives the footer for a specific post
postFooter :: Text.Text -> PostMeta -> Text.Text
postFooter baseUrl post = Text.concat
  ["<span class=\"filed\">Filed under: </span><ul>\n"
  , categoryLinks baseUrl post, "</ul>\n"]


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
