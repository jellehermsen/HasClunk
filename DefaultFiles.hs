{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Html
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen, 2015
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module DefaultFiles (defaultFile) where

import qualified Data.Text        as Text
import Text.RawString.QQ


-------------------------------------------------------------------------------
-- | 'defaultFile' gives the file contents for usage during initialization
defaultFile :: Text.Text -> Text.Text
defaultFile "config"  = config
defaultFile "css"     = css
defaultFile "footer"  = footer
defaultFile "header"  = header
defaultFile "post"    = post
defaultFile "contact" = contact


-------------------------------------------------------------------------------
config :: Text.Text
config = [r|# Amount of posts on your homepage
posts_on_home = 10

-- Commandline argument to convert your source files in pages/posts
# Use {in} and {out} for the source and target file
convert = pandoc -f markdown -w html -o {out} {in}

# The url of the website, mind the trailing /
url = http://jelle.aard.xyz/test/

# The title of the website (used in the RSS)
title = HasClunk blog

# The description of the blog (used in the RSS)
description = A blog, ambivalently generated by HasClunk

# The extension for your posts/pages (defaults to .md)
extension = .md
|]


-------------------------------------------------------------------------------
post :: Text.Text
post = [r|<!--
title: Hello world
categories: example category
-->
# Corporis quae ossa freta quaeratur semper

## Quod eheu licet

Lorem markdownum femina quibus Berecyntius: est, iurgia fecit. Ascendere pater,
caveo *feritatis*: levis procellamnos clipeo petere oppressos Alpino pavet?
Denique decuit, antro fons servitura animum metuit est, crimen *est*. Cerae
sortita cedere circumque forti sacrilegae!

<!--more-->

## Nurus quoque dextra

Preces est nec est montis quibus numeroque nomen. Flexerat taedae, solebant, si
vix percussit ignes stant urbem vocoque vibrantia erat dicam dubitas inposuit,
nec! Nec nunc expulit, hunc faciam, et senectus nostra annua, haud saepe
secedit, gesserat sorori tetigisse.

- In niveo littera
- Genu regionibus reges agmine omen plurima
- Parvae quod Iphis
- Tellurem tutaque extensus dabatur
- Et dextra virga

## Rasilis occursu mea illa iam

[Honor](http://jelle.aard.xyz/) ille rapta nec poteram fonte nam ad parili retia
crescere corpus tenent. Miserata Hippasus *ubi Ultor corpus* tormenta, Telchinas
querellas Lesbi, iaculum sequerere quamquam habitus mecum desideret illa; cum!

> Sub pertimuitque nec fovebam iacet quoque, ad regia, cuspide quaeque,
> penetraret largoque tumulum capiunt vetustas perpetuo. Et vultu imitante sunt
> levatae occuluit, clausit **nubes**, vidisse. Tauros postquam. Temptamenta
> bacis in surgere quatere essent mulcendas removerat, fons refer nobis,
> sparsitque ex [solus nec](http://jelle.aard.xyz/).

## Rector quam quoniam conata

Et sive, dextra, ire gradu ducis Achilles obicit, finem Euboicam sanguineae
ignes opibus quae bellica minas et? **Ipse una rapacibus** in aura vix una
superasque virginea facis [in protulit dicturus](http://aard.xyz) me illa:
remos Cenaeo. Maeoniam se iam liceret inducere habitantque sors aut sed veste
sunt ille virgo, moenia furor insidias. Et est a positaque mundi ipsa: igne et
ille exactum.
|]


-------------------------------------------------------------------------------
contact :: Text.Text
contact = [r|<!--
title: Contact
-->
You can contact me through the interwebs.
|]


-------------------------------------------------------------------------------
header :: Text.Text
header = [r|<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <meta name="viewport" content="initial-scale=1, maximum-scale=1">
    <link href="{base_url}assets/style.css" rel="stylesheet" />
    <link rel="alternate" type="application/rss+xml"
      title="RSS Feed for {title}" href="{base_url}feed.xml" />
    <title>HasClunk blog</title>
</head>
<body class="{pageType}">
    <header>
        <h1><a href="{base_url}">{title}</a></h1>
        <a href="{base_url}feed.xml" class="rss"/>RSS</a>
        <br class="clearfix"/>
    </header>
    <nav>
        <ul>
            <li><a href="{base_url}">Home</a></li>
            <li><a href="{base_url}archive.html">Archive</a></li>
            <li><a href="{base_url}pages/contact.html">Contact</a></li>
        </ul>
    </nav>
    <main>
|]


-------------------------------------------------------------------------------
footer :: Text.Text
footer = [r|
    </main>
    <footer>
        <div class="copyright">&copy; 2015</div>
        <div class="hasclunk">
            Weblog ambivalently powered by
            <a href="https://github.com/jellehermsen/HasClunk">HasClunk</a>
        </div>
    </footer>
</body>
</html>|]


-------------------------------------------------------------------------------
css :: Text.Text
css = [r|
html, body {
    font-family: monospace;
    font-size: 18px;
    margin: 0px;
    padding: 0px;
    text-align: center;
    color: #777;
}

a {
    color: #777;
    text-decoration: none;
    border-bottom: dotted 1px #999;
}

br.clearfix {
    clear: both;
}

h1, h2, h3, h4, h5, h6 {
    color: #000;
    font-weight: normal;
}

/* --- Main --- */
main {
   position: relative;
   margin: 0 auto;
   width: 700px;
   text-align: left;
   padding-bottom: 30px;
}

/* --- Header --- */
header {
    min-height: 40px;
    width: auto;
    clear: both;
    background-color: #FFF;
    padding: 0px 10px 0px 10px;
    border-bottom: solid 1px #CCC;
    text-align: center;
}

header h1 {
    float: left;
    line-height: 40px;
    margin: 0px 10px 0px 0px;
    padding: 0px;
    font-weight: normal;
    color: #777;
}

header a.rss {
    float: right;
    line-height: 40px;
    margin: 0px;
    padding: 0px;
    border-bottom: none;
}

header h1 a {
    border-bottom: none;
}

/* --- Footer --- */
footer {
    width: auto;
    padding: 0px 10px 0px 10px;
}

footer div.copyright {
    float: left;
    font-size: 13px;
}

footer div.hasclunk {
    float: right;
    font-size: 13px;
}

/* --- Navigation --- */
nav ul {
    list-style: none;
    display: inline;
    margin: 0px;
    padding: 0px;
    clear: both;
}

nav li {
    margin-right: 10px;
    display: inline;
    line-height: 40px;
}

nav li a {
    font-size: 24px;
    border-bottom: none;
}

/* --- Article --- */
article {
    background-color: #FFF;
    border: solid 1px #CCC;
    margin: 30px auto;
    max-width: 600px;
    text-align: left;
}

article h1:first-child {
    margin-top: 0px;
    padding-top: 0px;
}

article section {
    padding: 30px;
}

article header {
    width: auto;
    height: auto;
}

article:nth-child(5n+0) header, article:nth-child(5n+0) footer {
    background: #A7BED3;
}

article:nth-child(5n+1) header, article:nth-child(5n+1) footer {
    background: #C6E2E9;
}

article:nth-child(5n+2) header, article:nth-child(5n+2) footer {
    background: #F1FFC4;
}

article:nth-child(5n+3) header, article:nth-child(5n+3) footer {
    background: #FFCAAF;
}

article:nth-child(5n+4) header, article:nth-child(5n+4) footer {
    background: #DAB894;
}

article header h1 {
    text-align: left;
}

article header::after {
    clear: both;
    width: 100%;
    height: 0px;
    content: "";
    display: block;
}

article footer::after {
    clear: both;
    height: 0px;
    content: "";
    display: block;
}

article footer ul {
    list-style: none;
    display: inline;
    margin: 0px;
    padding: 0px;
}

article footer li {
    display: inline;
    margin-right: 10px;
    line-height: 40px;
}

article header time {
    float: right;
    display: block;
    line-height: 40px;
}

article.categories ul, article.archive ul {
    margin-top: 0px;
}

article.categories li, article.archive li {
    margin-bottom: 10px;
}

article.archive h2:first-child {
    margin-top: 0px;
}

article a.readmore {
    clear: both;
}

/* --- Mobile -- */
@media screen and (max-width: 700px) {
    a.rss {
        display: none;
    }

    nav li a {
        font-size: 18px;
        border-bottom: none;
    }

    article section {
        padding: 10px;
    }

    footer div.copyright {
        float: left;
        clear: both;
        font-size: 13px;
    }

    footer div.hasclunk {
        float: left;
        clear: both;
        font-size: 13px;
        text-align: left;
    }

    main {
        width: 100%;
    }
}
|]
