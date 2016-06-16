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
module BitStream
    ( BitStream
    , makeStream
    , getBits
    , consume
    , getAndConsume
    , BitStream.isEmpty
    , getRest
    ) where

import Control.Monad.State.Strict
import Data.Bits
import Data.ByteString
import Data.ByteString.Char8
import Data.Char

-- |Opaque data type that contains a bitstream
data BitStream = BitStreamy
    { bytes  :: !ByteString
    , number :: !Int
    , offset :: !Int
    } deriving (Show)

-- |Make a bitstream out of a ByteString
makeStream :: ByteString -> BitStream
makeStream theBytes = BitStreamy
    { bytes=theBytes
    , number=0
    , offset=0 }

getBits_ :: BitStream -> Int -> (BitStream, Int)
getBits_ stream numBits =
    if numBits > offset stream
        then getBits_ newStream numBits
        else (consumedStream, bits)
  where
    newStream = stream { number=number stream .|. byte `shiftL` offset stream,
                         offset=offset stream + 8,
                         bytes=Data.ByteString.Char8.tail $ bytes stream }
    byte = ord . Data.ByteString.Char8.head $ bytes stream
    consumedStream = stream -- { number=newNumber }
    newNumber = if numBits == 32
                    then 0
                    else number stream `shiftR` numBits
    bits = number stream .&. ((1 `shiftL` numBits) - 1)

-- |Return the specified number of bits from a BitStream,
-- converted to an integer using big endian coding
getBits :: Int -> State BitStream Int
getBits 0       = return 0
getBits numBits = do
    stream <- get
    let (newstream, result) = getBits_ stream numBits
    put newstream
    return result

consume_ :: BitStream -> Int -> BitStream
consume_ stream numBits =
    stream { offset=offset stream - numBits,
             number=if numBits == 32
                then 0
                else number stream `shiftR` numBits }

-- |Consume the specified number of bits
consume :: Int -> State BitStream ()
consume numBits = do
    stream <- get
    put $ consume_ stream numBits

getAndConsume_ :: BitStream -> Int -> (BitStream, Int)
getAndConsume_ stream numBits = (newStream, bits)
    where
        (stream', bits) = getBits_ stream numBits
        newStream = consume_ stream' numBits

-- |A combination of the getBits and consume functions
getAndConsume :: Int -> State BitStream Int
getAndConsume 0       = return 0
getAndConsume numBits = do
    stream <- get
    let (newstream, bits) = getAndConsume_ stream numBits
    put newstream
    return bits

-- | Is the BitStream empty?
isEmpty :: State BitStream Bool
isEmpty = (== empty) . bytes <$> get

-- | Return a new BitStream that contains the unread
-- rest of the given BitStream.
getRest :: State BitStream BitStream
getRest = get