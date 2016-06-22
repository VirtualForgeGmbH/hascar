{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module: FlatedFile
-- Copyright: (C) 2015-2016, Virtual Forge GmbH
-- License: GPL2
-- Maintainer: Hans-Christian Esperer <hans-christian.esperer@virtualforge.com>
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
module Codec.Archive.SAPCAR.FlatedFile
    ( decompressBlock
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Array.Base
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Char
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.Sequence ((><), (|>))
import Data.STRef
import Data.Word
import System.IO

import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short.Internal as SB
import qualified Data.Sequence as DS

import Codec.Archive.SAPCAR.BitStream
import Codec.Archive.SAPCAR.CanonicalHuffmanTree
import Codec.Archive.SAPCAR.FlexibleUtils

import Debug.Trace

-- Copied from vpa108csulzh.cpp under GPL by SAP AG
border :: [Int]
border = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

cplens :: [Int]
cplens = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43,
          51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0]

cpdist :: [Int]
cpdist = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257,
          385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289,
          16385, 24577]

csExtraDistBits :: [Int]
csExtraDistBits = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7,
                   8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13]

csExtraLenBits :: [Int]
csExtraLenBits = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3,
                  3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99]
-- End copied from vpa108csulzh.cpp under GPL by SAP AG

data OutStream s = OutStream
    { osBuf     :: STUArray s Int Word8
    , osPos     :: STRef s Int }

readInt32Big :: Handle -> IO Int
readInt32Big h = do
    [b1, b2, b3, b4] <- replicateM 4 $ ord <$> hGetChar h
    return $ b1 * 16777216 + b2 * 65536 + b3 * 256 + b4

entryReader :: [[Int]]
                 -> CanonicalHuffmanTree
                 -> Int
                 -> Int
                 -> BitStream s
                 -> ST s [Int]
entryReader entries huft entriesToRead lastEntry stream
    | (length . concat $ entries) >= entriesToRead = return . concat . reverse $ entries
    | otherwise                                    = do
        entry <- readEntry huft stream
        newEntries <- handleEntry entry
        entryReader (newEntries:entries) huft entriesToRead (last newEntries) stream
  where
      handleEntry code
        | code < 16     = return [code]
        | code == 16    = do
            numRepetitions <- (3 +) <$> getAndConsume stream 2
            return $ replicate numRepetitions lastEntry
        | code == 17    = do
            numZeroes <- (3 +) <$> getAndConsume stream 3
            return $ replicate numZeroes 0
        | code == 18    = do
            numZeroes <- (11 +) <$> getAndConsume stream 7
            return $ replicate numZeroes 0
        | otherwise     = error "Corrupted file"
            
decodeIt
    :: CanonicalHuffmanTree
    -> CanonicalHuffmanTree
    -> BitStream s
    -> OutStream s
    -> ST s ()
-- decodeIt lt dt = BS.pack . toList <$> decodeIt' empty
decodeIt lt dt stream out = do
        entry <- readEntryRaw lt stream
        return ()
        case numExtraBits entry of
            n | n == eobcode    -> return ()
            n | n == litcode    -> writeOut out (fromIntegral $ value entry) >>
                decodeIt lt dt stream out
            n | n >  litcode    -> error "Sonderfall not handled"
            _             -> do
                -- n <- (+ value entry) <$> getAndConsume (numExtraBits entry - 16)
                n <- (+ value entry) <$> getAndConsume stream (numExtraBits entry)
                distEntry <- readEntryRaw dt stream
                dist <- (+ value distEntry) <$> getAndConsume stream (numExtraBits distEntry)
                return ()
                copyBytes out dist n
                decodeIt lt dt stream out

writeOut :: OutStream s -> Word8 -> ST s ()
writeOut s b = do
    pos <- readSTRef $ osPos s
    writeArray (osBuf s) pos b
    writeSTRef (osPos s) $ pos + 1

copyBytes :: OutStream s -> Int -> Int -> ST s ()
copyBytes buf dist len = do
    minPos <- (subtract dist) <$> readSTRef (osPos buf)
    copyBytes' buf minPos $ minPos + len

copyBytes' :: OutStream s -> Int -> Int -> ST s ()
copyBytes' s n m
    | n < m    = do
        readArray (osBuf s) n >>= writeOut s
        copyBytes' s (n + 1) m
    | otherwise = return ()

-- |Decompress one or more lzh compressed blocks
decompressBlock :: BS.ByteString -> BS.ByteString
decompressBlock c = BS.take n . SB.fromShort $ SB.SBS a
    where
        (n, array)          = decompressBlock' c
        (!UArray _ _ _ a)    = array

decompressBlock' :: BS.ByteString -> (Int, UArray Int Word8)
decompressBlock' inp = runST $ do
    stream <- makeStream inp
    o <- decompressor stream
    o' <- freeze $ osBuf o
    l <- readSTRef $ osPos o
    return (l, o')

skipNonsenseBits :: BitStream s -> ST s ()
skipNonsenseBits stream = do
    numNonsenseBits <- getAndConsume stream 2
    when (numNonsenseBits > 0) $
        void $ getAndConsume stream numNonsenseBits

makeOutStream :: Int -> ST s (OutStream s)
makeOutStream len = OutStream
    <$> (newArray (0, len - 1) 0 :: ST s (STUArray s Int Word8))
    <*> newSTRef 0

decompressor :: BitStream s -> ST s (OutStream s)
decompressor s = do
    skipNonsenseBits s
    o <- makeOutStream 65536
    decompressor' s o
    return o


decompressor' :: BitStream s -> OutStream s -> ST s ()
decompressor' stream out = do
    lastBlock <- getAndConsume stream 1
    blockType <- getAndConsume stream 2
    res <- case blockType of
        1 -> decompressStaticBlock stream out
        2 -> decompressDynamicBlock stream out
        _ -> error $ "Block type " ++ show blockType ++ " not supported!"
    case lastBlock of
        1 -> return ()
        0 -> decompressor' stream out

decompressDynamicBlock :: BitStream s -> OutStream s -> ST s ()
decompressDynamicBlock stream out = do
    numLiterals <- (+ 257) <$> getAndConsume stream 5
    numDistanceCodes <- (+ 1) <$> getAndConsume stream 5
    numBitLengths <- (+ 4) <$> getAndConsume stream 4
    let bitLengthPositions = Prelude.take numBitLengths border
    bitLengths' <- mapM (\blp -> (,) blp <$> getAndConsume stream 3) bitLengthPositions
    let bitLengths = makeFlexList (0, 18) 0 bitLengths'
        huft = makeHuffmanTree bitLengths 19 [] []
        entriesToRead = numLiterals + numDistanceCodes
    ll <- entryReader [] huft entriesToRead (-1) stream
    let lengthCodes = take numLiterals ll
        distCodes = take numDistanceCodes $ drop numLiterals ll
        lengthTree = makeHuffmanTree lengthCodes 257 cplens csExtraLenBits
        distTree = makeHuffmanTree distCodes 0 cpdist csExtraDistBits
    return ()
    decodeIt lengthTree distTree stream out

staticLengthTree = makeHuffmanTree lengthCodes 257 cplens csExtraLenBits
    where
        -- Length and dist codes copied from vpa108csulzh.cpp under GPL by SAP AG
        lengthCodes = replicate 144 8 ++ replicate 112 9 ++ replicate 24 7 ++ replicate 8 8
        -- End length and dist codes copied from vpa108csulzh.cpp under GPL by SAP AG

staticDistTree = makeHuffmanTree distCodes 0 cpdist csExtraDistBits
    where
        -- Length and dist codes copied from vpa108csulzh.cpp under GPL by SAP AG
        distCodes   = replicate 30 5
        -- End Length and dist codes copied from vpa108csulzh.cpp under GPL by SAP AG

decompressStaticBlock :: BitStream s -> OutStream s -> ST s ()
decompressStaticBlock = decodeIt staticLengthTree staticDistTree

