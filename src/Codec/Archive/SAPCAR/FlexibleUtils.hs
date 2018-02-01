-- |
-- Module: FlexibleUtils
-- Copyright: (C) 2015-2018, Virtual Forge GmbH
-- License: GPL2
-- Maintainer: Hans-Christian Esperer <hc@hcesperer.org>
-- Stability: experimental
-- Portability: portable
-- |
-- (De-)compress SAPCAR files

module Codec.Archive.SAPCAR.FlexibleUtils
    ( makeFlexArray
    , makeFlexList
    ) where

import           Control.Monad.ST
import qualified Data.Array.ST    as FLEX
import           GHC.Arr

-- |Make an array based on a range, a default value and a list of
-- tuples that can be used to initialise specific entries of the
-- array.
makeFlexArray :: (Int, Int) -> a -> [(Int, a)] -> GHC.Arr.Array Int a
makeFlexArray arange defval pairs = FLEX.runSTArray $ do
            bl <- FLEX.newArray arange defval -- :: ST s (FLEX.STArray s Int a)
            mapM_ (uncurry $ FLEX.writeArray bl) pairs
            return bl

-- |Make a flexible array and convert it to a list using the
-- natural order of the array.
makeFlexList :: (Int, Int) -> Int -> [(Int, Int)] -> [Int]
makeFlexList a b c = elems $ makeFlexArray a b c
