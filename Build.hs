{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Build
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen, 2015
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module Build (build) where

import qualified Data.List        as List
import qualified Data.Maybe       as Maybe
import qualified Data.Text        as Text
import qualified Data.Text.IO     as IO
import qualified System.Directory as Directory
import qualified System.Cmd       as Cmd
import           Helpers
import           Html
import           PostMeta
import           Rss


-------------------------------------------------------------------------------
-- | 'build' builds the current blog, wipes out website/ in the process
build :: IO ()
build = do
    putStrLn $ Text.unpack "Loading please wait..."
    putStrLn $ Text.unpack "Building your new blog"
    -- Get the config file and lists of pages and posts
    rawConfig  <- IO.readFile "config"
    rawPosts   <- Directory.getDirectoryContents $ Text.unpack "posts"
    rawPages   <- Directory.getDirectoryContents $ Text.unpack "pages"


    -- Weed out all the non-markdown files
    let posts   = filterFiles rawPosts
    let pages   = filterFiles rawPages

    -- Parse the config file
    let config  = parseConfig rawConfig
    let url     = justOrError (lookup "url" config)
            "Could not find 'url' in your config"
    let title   = justOrError (lookup "title" config)
            "Could not find 'title' in your config"
    let description = justOrError (lookup "description" config)
            "Could not find 'description' in your config"
    let postsOnHome = Helpers.textToInt
            $ justOrError (lookup "posts_on_home" config)
            "Could not find 'posts_on_home' in your config"
    let baseUrl = justOrError (lookup "url" config)
            "Could not find 'url' in your config"

    -- Create all the markdown conversion commands
    let convertCmd = justOrError (lookup "convert" config)
            "Could not find 'convert' in your config"
    let postCmds =
            map (convertFile "posts/" "website/posts/" convertCmd) posts
    let pageCmds =
            map (convertFile "pages/" "website/pages/" convertCmd) pages


    -- Retrieve the header and footer files
    headerRaw  <- IO.readFile "template/header.html"
    footerRaw  <- IO.readFile "template/footer.html"
    let header = Text.replace "{title}" title
            $ Text.replace "{base_url}" url headerRaw
    let footer = Text.replace "{title}" title
            $ Text.replace "{base_url}" url footerRaw

    -- Remove the website directory and create a fresh one
    Directory.removeDirectoryRecursive "website"
    mapM_ Directory.createDirectory
      ["website", "website/pages", "website/posts", "website/assets"
      , "website/categories"]

    -- Convert all the posts and pages into html
    mapM_ Cmd.system $ postCmds ++ pageCmds

    -- Get the metadata for all the posts
    meta <- mapM getPostMeta posts
    let metaSorted = List.sortBy (\x y -> compare (date y) (date x)) meta

    -- Get page titles
    pageTitles <- mapM getPageTitle pages

    -- Add post headers to all the posts
    putStrLn $ Text.unpack "Adding postheader to posts"
    mapM_ (addPostHtml baseUrl) metaSorted

    -- Add page headers to all the pages
    putStrLn $ Text.unpack "Adding pageheaders to pages"
    mapM_ addPageHtml pageTitles

    putStrLn $ Text.unpack "Adding templates to files"
    -- Add templates to all the html files
    mapM_ (addTemplate header footer "website/posts/") posts
    mapM_ (addTemplate header footer "website/pages/") pages

    -- Copy all the assets to the website
    -- I've looked at doing this in Haskell, but this leads
    -- to ugly solutions like this: http://is.gd/PMdxUb
    Cmd.system "cp -R assets/* website/assets/"

    -- Create the category files
    let catList  = categoryList metaSorted
    let catFiles = map (categoryFile baseUrl header footer) catList

    mapM_ (\x -> IO.writeFile (Text.unpack $ Text.append "website/" $ fst x)
      (snd x)) catFiles

    -- Create the archive
    IO.writeFile "website/archive.html" $ Text.concat
      [header, archive baseUrl catList metaSorted, footer]

    -- Create the rss feed
    IO.writeFile "website/feed.xml" $ rss metaSorted title url description

    -- Create the index
    IO.writeFile "website/index.html"
      $ Text.concat [header, Html.index baseUrl
      $ take postsOnHome metaSorted, footer]


-------------------------------------------------------------------------------
-- | 'getPostMeta' parses a post's metadata from the converted html
getPostMeta :: Text.Text -> IO PostMeta
getPostMeta file = do
    content <- IO.readFile $ (Text.unpack "posts/") ++ (Text.unpack file)
    html    <- IO.readFile $ (Text.unpack "website/posts/")
                 ++ (Text.unpack $ htmlExt $ noExt file)

    let metaList = map parseMetaLine $ filter (not . Text.null) $ Text.lines
                     $ Text.replace "<!--" "" $ fst
                     $ Text.breakOn "-->" html
    return (PostMeta (title metaList)
      (date file) (noExt file) (categories metaList) html)

    where
        title m         = justOrError (lookup "title" m)
                            $ "Title not defined in " `Text.append` file

        categories m    = map Text.strip $ Text.splitOn ","
                            $ justOrEmpty (lookup "categories" m)

        parseMetaLine t = (Text.strip $ fst $ breakOnColon t, Text.strip
                            $ Text.tail $ snd $ breakOnColon t)

        breakOnColon    = Text.breakOn ":"

        -- todo: Add checks for the date format
        date f          = Text.tail $ Text.concat $ map (Text.append "-")
                            $ take 3 (Text.splitOn "-" file) -- ugly


-------------------------------------------------------------------------------
-- | 'getPageTitle' parses a page's metadata from the converted html
getPageTitle :: Text.Text -> IO (Text.Text, Text.Text)
getPageTitle file = do
    content <- IO.readFile $ (Text.unpack "pages/") ++ (Text.unpack file)
    html    <- IO.readFile $ (Text.unpack "website/pages/") ++ (Text.unpack
      $ htmlExt $ noExt file)

    let metaList = map parseMetaLine $ filter (not . Text.null) $ Text.lines
                     $ Text.replace "<!--" "" $ fst
                     $ Text.breakOn "-->" html

    return (file, title metaList)
    where
        title m         = justOrError (lookup "title" m)
                            $ "Title not defined in " `Text.append` file
        parseMetaLine t = (Text.strip $ fst $ breakOnColon t, Text.strip
                            $ Text.tail $ snd $ breakOnColon t)
        breakOnColon    = Text.breakOn ":"

-------------------------------------------------------------------------------
-- | 'addTemplate' adds the footer ander header to a file
addTemplate :: Text.Text -> Text.Text ->  Text.Text -> Text.Text -> IO ()
addTemplate header footer directory file = do
    content <- IO.readFile path
    IO.writeFile path $ Text.concat [header, content, footer]
    where
        path = Text.unpack $ directory `Text.append` (htmlExt $ noExt file)


-------------------------------------------------------------------------------
-- | 'addPostHtml' adds the header/footer to a post
addPostHtml :: Text.Text -> PostMeta -> IO ()
addPostHtml baseUrl post = 
    IO.writeFile path $ postHtml baseUrl False post
    where
        path = Text.unpack $ Text.append "website/posts/"
          $ htmlExt $ fileName post


-------------------------------------------------------------------------------
-- | 'addPageHtml' adds the header/footer to a page
addPageHtml :: (Text.Text, Text.Text) -> IO ()
addPageHtml (filename, title) = do
    content <- IO.readFile path
    IO.writeFile path $ pageHtml content title filename
    where
        path = Text.unpack $ Text.append "website/pages/" $ htmlExt
          $ noExt filename


-------------------------------------------------------------------------------
-- | 'categoryFile' set up the category html file
categoryFile :: Text.Text -> Text.Text -> Text.Text -> (Text.Text, [PostMeta])
                -> (Text.Text, Text.Text)
categoryFile baseUrl header footer cat =
  (Text.concat ["categories/", fst cat, ".html"]
  , Text.concat [header, categoryIndex baseUrl cat, footer])


-------------------------------------------------------------------------------
-- | 'categoryList' creates a list of categories, with all the PostMetas for
--   every category
categoryList :: [PostMeta] -> [(Text.Text, [PostMeta])]
categoryList meta = List.sortBy (\x y -> compare (fst x) (fst y))
  $ sortCategoryPosts $ mergeCategories
  $ concat $ map categoriesForPost meta


-------------------------------------------------------------------------------
-- | 'categoriesForPost' lists the categories for a given post
categoriesForPost :: PostMeta -> [(Text.Text, [PostMeta])]
categoriesForPost meta = map (\x -> (x, [meta])) $ categories meta


-------------------------------------------------------------------------------
-- | 'mergeCategories' merges all the categories for
--   [(categoryName, [PostMeta])]
mergeCategories :: [(Text.Text, [PostMeta])] -> [(Text.Text, [PostMeta])]
mergeCategories cats = foldl mergeCategory [] cats


-------------------------------------------------------------------------------
-- | 'sortCategoryPosts' sorts all the post in the given categories
sortCategoryPosts :: [(Text.Text, [PostMeta])] -> [(Text.Text, [PostMeta])]
sortCategoryPosts cats = map (\x -> (fst x, sort $ snd x)) cats
    where
        sort posts = List.reverse $ List.sortBy
          (\a b -> compare (date a) (date b)) posts


-------------------------------------------------------------------------------
-- | 'mergeCategory' merges a list of categories
mergeCategory :: [(Text.Text, [PostMeta])] -> (Text.Text, [PostMeta])
  -> [(Text.Text, [PostMeta])]
mergeCategory cats cat = case lookup (fst cat) cats of
                             Maybe.Just c -> replaceTuple cats (fst cat, c)
                               (fst cat, (snd cat) ++ c)

                             Nothing      -> (fst cat, snd cat) : cats


-------------------------------------------------------------------------------
-- | 'filterFiles' removes all non-markdown files from a list
filterFiles :: [FilePath] -> [Text.Text]
filterFiles = filter (Text.isSuffixOf ".md") . map Text.pack


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
-- | 'convertFile' transforms the convert_markdown command in the config
-- | into a command that will convert a .md file from the source directory
-- | to a .html file in the target directory
convertFile :: Text.Text -> Text.Text -> Text.Text -> Text.Text -> String
convertFile srcDir targetDir convertCmd file
    = Text.unpack $ Text.replace "{out}"
                      (Text.append targetDir $ htmlExt $ noExt file)
                  $ Text.replace "{in}"
                      (srcDir `Text.append` file) convertCmd
