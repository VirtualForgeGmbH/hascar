{-# LANGUAGE RankNTypes #-}
-- |
-- Module: BitStream
-- Copyright: (C) 2015, Virtual Forge GmbH
-- License: GPL@
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
module Codec.Archive.SAPCAR.BitStream
    ( BitStream
    , makeStream
    , getBits
    , consume
    , getAndConsume
    , Codec.Archive.SAPCAR.BitStream.isEmpty
    ) where

import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Array.MArray
import Data.Array.ST
import Data.Bits
import Data.ByteString
import Data.ByteString.Char8
import Data.Char
import Data.STRef
import Data.Word

import Debug.Trace

import qualified Data.ByteString as S

-- |Opaque data type that contains a bitstream
data BitStream s = BitStreamy
    { bytes     :: STUArray s Int Word8
    , len       :: Int
    , number    :: STRef s Int
    , offset    :: STRef s Int
    , position  :: STRef s Int
    }

-- |Make a bitstream out of a ByteString
makeStream :: ByteString -> ST s (BitStream s)
makeStream theBytes = do
    array <- newArray (0, S.length theBytes) 0 :: ST s (STUArray s Int Word8)
    mapM_ (\i -> writeArray array i $ S.index theBytes i) [0..(S.length theBytes - 1)]
    number <- newSTRef 0
    offset <- newSTRef 0
    position <- newSTRef 0
    return $ BitStreamy
        { bytes=array
        , len=S.length theBytes
        , number=number
        , offset=offset
        , position=position }

-- |Return the specified number of bits from a BitStream,
-- converted to an integer using big endian coding
getBits :: BitStream s -> Int -> ST s Int
getBits _ 0 = return 0
getBits stream numBits = do
    offs <- readSTRef $ offset stream
    num  <- readSTRef $ number stream
    if numBits > offs
        then do
            pos <- readSTRef $ position stream
            newByte <- fromIntegral <$> readArray (bytes stream) pos
            writeSTRef (position stream) $ pos + 1
            let num' = num .|. newByte `shiftL` offs
            writeSTRef (offset stream) $ offs + 8
            writeSTRef (number stream) num'
            getBits stream numBits
        else let bits = num .&. ((1 `shiftL` numBits) - 1)
             in  return bits

-- |Consume the specified number of bits
consume :: BitStream s -> Int -> ST s ()
consume stream numBits = do
    modifySTRef (offset stream) $ subtract numBits
    modifySTRef (number stream) $ \n -> if numBits == 32 then 0 else n `shiftR` numBits

-- |A combination of the getBits and consume functions
getAndConsume :: BitStream s -> Int -> ST s Int
getAndConsume stream numBits = do
    res <- getBits stream numBits
    consume stream numBits
    return res

-- | Is the BitStream empty?
isEmpty :: BitStream s -> ST s Bool
isEmpty bs = (==) (len bs) <$> readSTRef (position bs)
