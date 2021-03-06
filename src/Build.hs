{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Build
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen
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
import qualified System.Process   as Process
import qualified Config           as Config
import           System.Directory(copyFile)
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

    -- Parse the config file
    let config  = Config.getConfig rawConfig

    -- Weed out all the non-markdown files
    let posts   = filterFiles (Config.extension config) rawPosts
    let pages   = filterFiles (Config.extension config) rawPages

    -- Create all the markdown conversion commands
    let postCmds =
            map (convertFile "posts/" "website/posts/" (Config.convertCmd config)) posts
    let pageCmds =
            map (convertFile "pages/" "website/pages/" (Config.convertCmd config)) pages

    -- Retrieve the header and footer files
    headerRaw  <- IO.readFile "template/header.html"
    footerRaw  <- IO.readFile "template/footer.html"
    let header = Text.replace "{title}" (Config.title config)
            $ Text.replace "{base_url}" (Config.url config) headerRaw
    let footer = Text.replace "{title}" (Config.title config)
            $ Text.replace "{base_url}" (Config.url config) footerRaw

    -- Remove the website directory and create a fresh one
    Directory.removeDirectoryRecursive "website"
    mapM_ Directory.createDirectory
      ["website", "website/pages", "website/posts", "website/assets"
      , "website/categories"]

    -- Convert all the posts and pages into html
    mapM_ Process.callCommand $ postCmds ++ pageCmds

    -- Get the metadata for all the posts
    meta <- mapM getPostMeta posts
    let metaSorted = List.sortBy (\x y -> compare (date y) (date x)) meta

    -- Get page titles
    pageTitles <- mapM getPageTitle pages

    -- Add post headers to all the posts
    putStrLn $ Text.unpack "Adding postheader to posts"
    mapM_ (addPostHtml config) metaSorted

    -- Add page headers to all the pages
    putStrLn $ Text.unpack "Adding pageheaders to pages"
    mapM_ addPageHtml pageTitles

    putStrLn $ Text.unpack "Adding templates to files"

    -- Add templates to all the html files
    mapM_ (addPostTemplate (pt "post" header) footer "website/posts/" config) (zip posts meta)
    mapM_ (addPageTemplate (pt "page" header) footer "website/pages/" config) pageTitles

    -- Copy all the assets to the website
    -- I've looked at doing this in Haskell, but this leads
    -- to ugly solutions like this: http://is.gd/PMdxUb
    Process.callCommand "cp -R assets/* website/assets/"


    -- little convenience function for replacing {head_title} and
    -- {meta_description} in html
    let replaceTitleDescr = (Text.replace "{head_title}" (Config.title config))
            . (Text.replace "{meta_description}" (Config.description config))

    -- Create the category files
    let catList  = categoryList metaSorted
    let catFiles = map (categoryFile config
            (pt "category" header) footer) catList

    mapM_ (\x -> IO.writeFile (Text.unpack $ Text.append "website/" $ fst x)
      (replaceTitleDescr (snd x))) catFiles

    -- Create the archive
    IO.writeFile "website/archive.html"
      $ replaceTitleDescr
      $ Text.concat [pt "archive" header,
        archive config catList metaSorted, footer]

    -- Create the rss feed
    IO.writeFile "website/feed.xml" $ rss metaSorted 
        (Config.title config) (Config.url config) (Config.description config)

    let blogHome = if (Config.homeIsPage config) then "website/blog.html"
                   else "website/index.html"

    -- Create the index
    if (Config.homeIsPage config) then
        copyFile "website/pages/home.html" "website/index.html"
    else
        return ()

    IO.writeFile blogHome
      $ replaceTitleDescr
      $ Text.concat [pt "index" header, Html.index config
      $ take (Config.postsOnHome config) metaSorted, footer]

    where
        pt x = Text.replace "{page_type}" x

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
      (date file) (noExt file) (categories metaList) html
      (description metaList))

    where
        title m         = justOrError (lookup "title" m)
                            $ "Title not defined in " `Text.append` file

        description m   = justOrEmpty (lookup "description" m)

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
-- | 'addPageTemplate' adds the footer ander header to a file, and replaces all
--   occurences of {page_name}, {meta_description} and {title}
addPageTemplate :: Text.Text -> Text.Text ->  Text.Text -> Config.Config
  -> (Text.Text, Text.Text) -> IO ()
addPageTemplate header footer directory config (file, title) = do
    content <- IO.readFile path
    IO.writeFile path $ Text.replace "{page_name}" fileName
        $ Text.replace "{head_title}" title
        $ Text.replace "{meta_description}" (Config.description config)
        $ Text.replace "{meta_description}" (Config.description config)
        $ Text.concat [header, content, footer]
    where
        fileName = noExt file
        path = Text.unpack $ directory `Text.append` (htmlExt fileName)

-------------------------------------------------------------------------------
-- | 'addPostTemplate' adds the footer ander header to a file, and replaces all
--   occurences of {page_name}, {meta_description} and {title}
addPostTemplate :: Text.Text -> Text.Text ->  Text.Text -> Config.Config
  -> (Text.Text, PostMeta) -> IO ()
addPostTemplate header footer directory config (file, meta) = do
    content <- IO.readFile path
    IO.writeFile path $ Text.replace "{meta_description}" metaDescription
        $ Text.replace "{head_title}" title
        $ Text.replace "{page_name}" fileName 
        $ Text.concat [header, content, footer]
    where
        fileName = noExt file
        pmd = PostMeta.description meta
        title = PostMeta.title meta
        metaDescription = if (pmd /= "") then pmd else Config.description config
        path = Text.unpack $ directory `Text.append` (htmlExt fileName)


-------------------------------------------------------------------------------
-- | 'addPostHtml' adds the header/footer to a post
addPostHtml :: Config.Config -> PostMeta -> IO ()
addPostHtml config post =
    IO.writeFile path $ postHtml config True False post
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
categoryFile :: Config.Config -> Text.Text -> Text.Text -> (Text.Text, [PostMeta])
                -> (Text.Text, Text.Text)
categoryFile config header footer cat =
  (Text.concat ["categories/", fst cat, ".html"]
  , Text.concat [header, categoryIndex config cat, footer])


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
filterFiles :: Text.Text -> [FilePath] -> [Text.Text]
filterFiles ext = filter (Text.isSuffixOf ext) . map Text.pack


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
