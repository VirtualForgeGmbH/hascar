-- |
-- Module: FlexibleUtils
-- Copyright: (C) 2015-2016, Virtual Forge GmbH
-- License: GPL2
-- Maintainer: Hans-Christian Esperer <hc@hcesperer.org>
-- Stability: experimental
-- Portability: portable
-- |
-- (De-)compress SAPCAR files
--
-- Copyright (C) 2016, Virtual Forge GmbH
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
-- USA
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
