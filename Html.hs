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
categoryIndex :: (Text.Text, [PostMeta]) -> Text.Text
categoryIndex (cat, posts) = Text.concat 
    ["<article class=\"categories\">\n"
     , "<header><h1>Category: ", cat, "</h1></header>\n"
     , "<section><ul>\n", listPosts posts, "</ul></section>\n"
     , "</article>"]


-------------------------------------------------------------------------------
-- | 'listPosts' generates a html list of posts (sans <ul>)
listPosts :: [PostMeta] -> Text.Text
listPosts = Text.concat . map listPost


-------------------------------------------------------------------------------
-- | 'listPost' creates a li with a link for a post
listPost :: PostMeta -> Text.Text
listPost (PostMeta t d f _ _) = Text.concat
    ["<li>"
    , "<a href=\"/posts/", htmlExt f, "\">"
    ,t," <time>(", d, ")</time></a>"
    , "</li>\n"]


-------------------------------------------------------------------------------
-- | 'archive' generates the Html for the archive page
archive :: [(Text.Text, [PostMeta])] -> [PostMeta] -> Text.Text
archive cats posts = Text.concat
    ["<article class=\"archive\">\n"
    , "<header><h1>Archive</h1></header>\n"
    , "<section><h2>Categories</h2>\n"
    , "<ul>\n", listCategories cats , "</ul>\n"
    , "<h2>Posts</h2>\n"
    , "<ul>\n", listPosts posts, "</ul>\n"
    , "</article>"]


-------------------------------------------------------------------------------
-- | 'categoryList' generates a html list of categories (sans </ul>)
listCategories :: [(Text.Text, [PostMeta])] -> Text.Text
listCategories cats = Text.concat $ map (\x -> Text.concat 
    ["<li><a href=\"/categories/", (fst x), ".html\">", (fst x), "</a></li>\n"])
    cats


-------------------------------------------------------------------------------
-- | 'index' gives all posts for display in the index.html
index :: [PostMeta] -> Text.Text
index posts = Text.concat $ map (postHtml True) posts


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
    ["<h1><a href=\"", link, "\">", title , "</a></h1>\n"]
    where
    link = Text.concat ["/posts/", htmlExt $ filename]


-------------------------------------------------------------------------------
-- | 'postHtml' adds header/footer..etc to a given post, you can specifiy
--   whether to display the entire post, or only an excerpt/teaser with a
--   "read more" link
postHtml ::  Bool -> PostMeta -> Text.Text
postHtml excerpt post = if excerpt && Text.isInfixOf "<!--more-->" content
    then
        Text.concat 
          ["<article>"
          , "<header>\n" , postHeader post,   "</header>\n"
          , "<section>\n", untilMore content, moreLink post, "</section>\n"
          , "<footer>\n" , postFooter post,   "</footer>"
          , "</article>\n\n"]
    else Text.concat 
          ["<article>"
          , "<header>\n" , postHeader post, "</header>\n"
          , "<section>\n", content,         "</section>\n"
          , "<footer>\n" , postFooter post, "</footer>\n"
          , "</article>\n\n"]
    where
        content = html post
        untilMore = fst . Text.breakOn "<!--more-->"
        moreLink p = Text.concat ["<a class=\"readmore\" href=\"/posts/"
          , htmlExt $ fileName p, "\">Read more...</a>"]


-------------------------------------------------------------------------------
-- | 'postHeader' gives the header for a specific post
postHeader :: PostMeta -> Text.Text
postHeader post = Text.concat 
    ["<h1><a href=\"", link, "\">", title post , "</a></h1>\n", "<time>" , date post, "</time>\n"]
    where
    link = Text.concat ["/posts/", htmlExt $ fileName post]


-------------------------------------------------------------------------------
-- | 'postFooter' gives the footer for a specific post
postFooter :: PostMeta -> Text.Text
postFooter post = Text.concat
  ["<span class=\"filed\">Filed under: </span><ul>\n", categoryLinks post, "</ul>\n"]


-------------------------------------------------------------------------------
-- | 'categoryLinks' creates a text with all the li's for the categories
--   that are linked to a post
categoryLinks :: PostMeta -> Text.Text
categoryLinks post = Text.concat $ map categoryLink $ categories post


-------------------------------------------------------------------------------
-- | 'categoryLink' creates a link to a category
categoryLink :: Text.Text -> Text.Text
categoryLink cat = Text.concat 
  ["<li><a href=\"/categories/", cat, ".html\">", cat, "</a></li>\n"]
