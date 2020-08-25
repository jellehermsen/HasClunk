{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Helpers
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module Helpers where

import qualified Data.Maybe     as Maybe
import qualified Data.Text      as Text


-------------------------------------------------------------------------------
-- | 'justOrError' takes a Maybe monad and returns
-- either the value, or exits the program with
-- a given error message
justOrError :: Maybe.Maybe a -> Text.Text -> a
justOrError Maybe.Nothing msg = error $ Text.unpack msg
justOrError (Maybe.Just x) _  = x


-------------------------------------------------------------------------------
-- | 'justOrEmpty' takes a Maybe monad and returns
-- its Text value, or an empty text
justOrEmpty :: Maybe.Maybe Text.Text -> Text.Text
justOrEmpty Maybe.Nothing  = ""
justOrEmpty (Maybe.Just x) = x


-------------------------------------------------------------------------------
-- | 'justOrDefault' takes a Maybe monad and returns
-- its value (in case of Maybe.Just), or the given default
justOrDefault :: Maybe.Maybe a -> a -> a
justOrDefault Maybe.Nothing def = def
justOrDefault (Maybe.Just x) _  = x


-------------------------------------------------------------------------------
-- | Replaces a tuple in a list of tuples, identified by the first element.
-- A more generalized version of http://stackoverflow.com/a/10474511
replaceTuple :: Eq a => [(a, b)] -> (a, b) -> (a, b) -> [(a, b)]
replaceTuple tups old new = map check tups where
    check tup | fst tup == fst old = new
              | otherwise  = tup


-------------------------------------------------------------------------------
-- | 'textToInt' Converts a given text to an int, amazeballs!
textToInt :: Text.Text -> Int
textToInt t = read $ Text.unpack t :: Int


-------------------------------------------------------------------------------
-- | 'textToBool' Converts a given text to an bool
textToBool :: Text.Text -> Bool
textToBool "0" = False
textToBool "1" = True
textToBool _   = False

-------------------------------------------------------------------------------
-- | 'htmlExt' appends ".html" to a given Text
htmlExt :: Text.Text -> Text.Text
htmlExt = flip Text.append ".html"


-------------------------------------------------------------------------------
-- | 'noExt' removes the extension form a filename
noExt :: Text.Text -> Text.Text
noExt = Text.init . fst . Text.breakOnEnd "."
