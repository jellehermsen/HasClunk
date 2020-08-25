{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Html
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module Init (initBlog) where

import Control.Monad (when)
import qualified Data.Text        as Text
import qualified Data.Text.IO     as IO
import qualified System.Directory as Directory
import Text.RawString.QQ
import DefaultFiles


-------------------------------------------------------------------------------
-- | 'initBlog' creates all the directories and dummy files for starting
--   a new blog
initBlog :: IO ()
initBlog = do
    dirContents <- Directory.getDirectoryContents "."
    if (length dirContents > 2)
        then
            putStrLn notEmptyError
        else
            do
                mapM_ Directory.createDirectory directories
                mapM_ (\x -> IO.writeFile (fst x) (snd x)) defaultFiles


-------------------------------------------------------------------------------
-- | 'directories' lists the directories that need to be created during
--   initialization
directories :: [FilePath]
directories = map Text.unpack
  ["assets", "pages", "posts", "template", "website"]


-------------------------------------------------------------------------------
-- | 'notEmptyError' gives the error message you get when you try to
--   initialize the blog inside a non-empty directory
notEmptyError :: String
notEmptyError = Text.unpack
  "Error: you should only use `HasClunk init` inside an empty directory"


-------------------------------------------------------------------------------
-- | 'defaultFiles' makes a list of all the files that need to be generated
--   during initialization
defaultFiles :: [(FilePath, Text.Text)]
defaultFiles = [
    ("config"                         , defaultFile "config"),
    ("template/header.html"           , defaultFile "header"),
    ("template/footer.html"           , defaultFile "footer"),
    ("posts/2020-08-25-hello-world.md", defaultFile "post"),
    ("assets/style.css"               , defaultFile "css"),
    ("pages/contact.md"               , defaultFile "contact")
  ]
