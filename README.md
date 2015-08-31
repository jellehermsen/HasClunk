HasClunk
========
An extremely simple static blog generator made in Haskell, licensed under BSD3

Installation
------------
Make sure you have ghc and cabal installed.

You can install this using:

    cabal install

Usage
-----
Initialize your blog inside an empty directory:

    hasclunk init

Generate your blog (result will go into website/)

    hasclunk build

Writing posts
-------------
You can write posts by simply putting files in the posts directory. Add the date
in front like so: 2015-07-14-teaser-post.md

By default HasClunk uses Pandoc to generate html from your source
files, but you could use any kind of system you like. Just modify the config
file and change the "convert" line.

At the top of your post you should include an html comment that gives some
metadata.  HasClunk will take this data from the generated html, so you can do
whatever trickery is needed to get it out of your source format. In markdown you
can simply add an html comment on top:

```html
<!--
title: Hello world
categories: categoryName
-->

```

You can use {base_url} in posts to indicate your blog's url defined in your config.

Writing pages
-------------
Writing new pages is mostly like writing posts, with the exceptions that you
don't need to tag the date on front, and you don't need to include categories in
the metadata.

Modifying the template
----------------------
HasClunk has two template files, template/header.html and template/footer.html.
All the generated html files will get smacked between these two files.

You can use {base_url} in these template files to refer to the url in your
config. There's also {title} you can drop in. These will get automatically
replaced during the build process.

You can use {page_type} inside you header.html. It will be replaced with the
type of page you're currently visiting (doh). You can use it for example in
body="{page_type}" to be able to add page-specific css trickery. The possible
values for page_type are: index, category, post and archive.

Help
----
```txt
Usage:: hasclunk [OPTION]

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

The configuration file has the following properties:
  posts_on_home  the amount of posts shown on the homepage
  convert        the shell command used to convert/move your posts by default 
                   is assumes you use markdown and it uses pandoc to convert
                   them to html, but you could even use a headless libreoffice
                   and convert your odt, if that tickles your fancy :-)
  url            the base-url of the website (mind the trailing /)
  title          the weblog title (for usage in RSS)

Posts need the following metadata on top in an html comment:
<!--
title: Post Title
categories: cat1, cat2, cat3
-->

Pages need the following metadata on top in an html comment:
<!--
title: Page title
-->
```
