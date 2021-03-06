{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Main
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module Main where

import qualified System.Environment as Environment
import           Text.RawString.QQ
import           Build
import           Init


-------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- Environment.getArgs
    if length args == 0
        then
            putStrLn "Please supply an argument, or --help for help"
        else
            case args !! 0 of
                "build"     -> build
                "init"      -> initBlog
                "--version" -> putStrLn version
                "--help"    -> putStrLn help
                _           -> putStrLn "Argument not recognized. Use --help for help"

-------------------------------------------------------------------------------
version :: String
version = "0.4.0.0"


-------------------------------------------------------------------------------
help :: String
help = [r|Usage:: HasClunk [OPTION]

HasClunk is a tiny, dependency light static blog generator.

  init        initializes the blog, you should use this in an empty directory
  build       builds your blog, blows away website/ in the process and
                puts everything there
  --version   shows the HasClunk version
  --help      the current help message

HasClunk generates a couple of directories for all your blogging pleasure:
  assets      this is where you put all your images/css/js and stuff
                it is recursively copied to website/assets. By default the
                css resides here.
  pages       you can put all your pages here, like contact, or posts you
                don't want to have visible inside your website. In order
                for people to see them, you have to link to them manually
                in the template, or from other pages/posts.
  posts       here you put all your posts, in the following format:
                YEAR-month-day-postname, i.e: 2015-07-21-Hello_World.md
  template    the heart and soul of the (cough) templating system,
                the generator takes header.html and footer.html from this
                directory and smacks your posts and pages in between
  website     this is where your generated website is put, each time you
                launch "build" it will blow away this directory

HasClunk generates a couple of files for your bloggy goodness:
  assets/style.css     the default css file
  config               the configuration file, read more below about this
  template/header.html the template header
  template/footer.html the template footer
  pages/contact.md     an example page
  posts/2015-07-14-teaser-post.md an example post

The configuration file has the following mandatory properties:
  posts_on_home   The amount of posts shown on the homepage
  convert         The shell command used to convert/move your posts by default 
                   is assumes you use markdown and it uses pandoc to convert
                   them to html, but you could even use a headless libreoffice
                   and convert your odt, if that tickles your fancy :-)
  url             The base-url of the website (mind the trailing /)
  title           The weblog title (for usage in RSS)
  description     The description for your blog. This is used in the RSS feed.

These configuration properties are optional:
  extension       Extension use for your content files (default is .md)
  show_categories Use 0 if you want to hide post categories (default is 1).
  home_is_page    Use 1 use a static page as your home (default is 0).
                   The blog index will be moved to /blog.html.
                   You can put the static home's contents in pages/home.md

  i8n_archive     Translation for "Archive"
  i8n_category    Translation for "Category:"
  i8n_categories  Translation for "Categories"
  i8n_filed_under Translation for "Filed under:"
  i8n_posts       Translation for "Posts"
  i8n_readmore    Translation for "Read more..."

Posts need the following metadata on top in an html comment:
<!--
title: Post Title
categories: cat1, cat2, cat3
-->

Pages need the following metadata on top in an html comment:
<!--
title: Page title
-->
|]
