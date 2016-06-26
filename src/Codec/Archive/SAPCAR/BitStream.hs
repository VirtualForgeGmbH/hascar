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
    return BitStreamy
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
    if numBits > offs
        then case numBits - offs of
            n | n < 9  -> refill False stream >> returnBits stream numBits
            n | n < 17 -> refill True  stream >> returnBits stream numBits
            _          -> refill True  stream >> getBits stream numBits
        else returnBits stream numBits

returnBits :: BitStream s -> Int -> ST s Int
returnBits stream numBits = do
    num <- readSTRef $ number stream
    return $ num .&. ((1 `shiftL` numBits) - 1)

refill :: Bool -> BitStream s-> ST s ()
{-# INLINE refill #-}
refill two stream = do
    pos <- readSTRef $ position stream
    num <- readSTRef $ number stream
    offs <- readSTRef $ offset stream
    if two
    then refillTwoBits pos num offs stream
    else refillOneBit  pos num offs stream

refillOneBit :: Int -> Int -> Int -> BitStream s -> ST s ()
{-# INLINE refillOneBit #-}
refillOneBit pos num offs stream = do
    newByte <- fromIntegral <$> readArray (bytes stream) pos
    writeSTRef (position stream) $ pos + 1
    let num' = num .|. newByte `shiftL` offs
    writeSTRef (offset stream) $ offs + 8
    writeSTRef (number stream) num'

refillTwoBits :: Int -> Int -> Int -> BitStream s -> ST s ()
{-# INLINE refillTwoBits #-}
refillTwoBits pos num offs stream = do
    newByte1 <- fromIntegral <$> readArray (bytes stream) pos
    newByte2 <- fromIntegral <$> readArray (bytes stream) (pos + 1)
    writeSTRef (position stream) $ pos + 2
    let num' = num .|. newByte1 `shiftL` offs .|. newByte2 `shiftL` (offs + 8)
    writeSTRef (offset stream) $ offs + 16
    writeSTRef (number stream) num'

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
