{-|
Module      : PostMeta
Description : A tiny, dependency light, static blog generator
Copyright   : (c) Jelle Hermsen
License     : BSD3
Maintainer  : j@jelle.xyz
Stability   : experimental
Portability : POSIX
-}
module PostMeta where

import qualified Data.Text as Text

-- | 'PostMeta' is used to store a post's metadata
-- | which is then used to populate the archive
-- | and category pages
data PostMeta = PostMeta {
    title       :: Text.Text,
    date        :: Text.Text,
    fileName    :: Text.Text, -- without extension
    categories  :: [Text.Text],
    html        :: Text.Text,
    description :: Text.Text
} deriving Show


