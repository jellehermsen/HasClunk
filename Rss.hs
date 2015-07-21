{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Rss
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen, 2015
License     : BSD3 
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module Rss (rss) where

import qualified Data.Text        as Text
import           PostMeta

rss :: [PostMeta] -> Text.Text -> Text.Text -> Text.Text -> Text.Text
rss posts title url description = 
    Text.concat $ ["<?xml version=\"1.0\"?>\n"
        , "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
        , "<channel>\n<title>",title,"</title>\n"
        , "<link>", url, "</link>\n"
        , "<atom:link href=\"", url, "feed.xml\" rel=\"self\" type=\"application/rss+xml\" />"
        , "<description>", description, "</description>"]
        ++ [Text.concat (map (rssPost url) posts)]
        ++ ["</channel>\n</rss>"]
        
rssPost :: Text.Text -> PostMeta -> Text.Text
rssPost url post = Text.concat ["<item>\n<title>", (title post), "</title>\n"
    , "<link>" , link, "</link>"
    , "<guid>", link, "</guid>\n"
    , "<description><![CDATA[", (html post), "]]></description>\n</item>\n"]
    where
        link = Text.concat [url, "posts/", Text.replace ".md" ".html" $ fileName post]
