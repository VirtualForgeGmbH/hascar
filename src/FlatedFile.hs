{-# LANGUAGE OverloadedStrings #-}
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
module FlatedFile
    ( decompressBlock
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.Sequence ((><), (|>))
import System.IO

import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Attoparsec.ByteString.Lazy as DABL
import qualified Data.Sequence as DS

import BitStream
import CanonicalHuffmanTree
import FlexibleUtils

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

readInt32Big :: Handle -> IO Int
readInt32Big h = do
    [b1, b2, b3, b4] <- replicateM 4 $ ord <$> hGetChar h
    return $ b1 * 16777216 + b2 * 65536 + b3 * 256 + b4

entryReader :: [[Int]]
                 -> CanonicalHuffmanTree
                 -> Int
                 -> Int
                 -> StateT
                      BitStream Data.Functor.Identity.Identity [Int]
entryReader entries huft entriesToRead lastEntry
    | (length . concat $ entries) >= entriesToRead = return . concat . reverse $ entries
    | otherwise                       = do
        entry <- readEntry huft
        newEntries <- handleEntry entry
        entryReader (newEntries:entries) huft entriesToRead $ last newEntries
  where
      handleEntry :: Int -> State BitStream [Int]
      handleEntry code
        | code < 16     = return [code]
        | code == 16    = do
            numRepetitions <- (3 +) <$> getAndConsume 2
            return $ replicate numRepetitions lastEntry
        | code == 17    = do
            numZeroes <- (3 +) <$> getAndConsume 3
            return $ replicate numZeroes 0
        | code == 18    = do
            numZeroes <- (11 +) <$> getAndConsume 7
            return $ replicate numZeroes 0
        | otherwise     = error "Corrupted file"
            
decodeIt :: CanonicalHuffmanTree -> CanonicalHuffmanTree -> State BitStream BS.ByteString
decodeIt lt dt = BS.pack . toList <$> decodeIt' empty
  where
    decodeIt' acc = do
        entry <- readEntryRaw lt
        case numExtraBits entry of
            n | n == eobcode    -> return acc
            n | n == litcode    -> decodeIt' $ acc |> (fromIntegral $ value entry)
            n | n >  litcode    -> error "Sonderfall not handled"
            _             -> do
                -- n <- (+ value entry) <$> getAndConsume (numExtraBits entry - 16)
                n <- (+ value entry) <$> getAndConsume (numExtraBits entry)
                distEntry <- readEntryRaw dt
                dist <- (+ value distEntry) <$> getAndConsume (numExtraBits distEntry)
                let new     = DS.drop (length acc - dist) acc
                    new'    = foldl (\a _ -> a >< new) empty [0..m]
                    m       = n `div` dist
                    l       = (acc >< (DS.take n new'))
                decodeIt' l
      

-- |Decompress one or more lzh compressed blocks
decompressBlock :: BS.ByteString -> [BS.ByteString]
decompressBlock inp = reverse blocks
    where
        blocks  = evalState decompressor . makeStream $ inp

skipNonsenseBits :: State BitStream ()
skipNonsenseBits = do
    numNonsenseBits <- getAndConsume 2
    when (numNonsenseBits > 0) $
        void $ getAndConsume numNonsenseBits

decompressor :: State BitStream [BS.ByteString]
decompressor = skipNonsenseBits >> decompressor' []

decompressor' :: [BS.ByteString] -> State BitStream [BS.ByteString]
decompressor' acc = do
    lastBlock <- getAndConsume 1
    blockType <- getAndConsume 2
    res <- case blockType of
        1 -> decompressStaticBlock
        2 -> decompressDynamicBlock
        _ -> error $ "Block type " ++ show blockType ++ " not supported!"
    (show (lastBlock, blockType)) `trace` return ()
    -- (show (lastBlock, blockType, res)) `trace` return ()
    case lastBlock of
        1 -> return (res:acc)
        0 -> decompressor' (res:acc)

decompressDynamicBlock :: State BitStream BS.ByteString
decompressDynamicBlock =  do
    numLiterals <- (+ 257) <$> getAndConsume 5
    -- ("Literals: " ++ show numLiterals) `trace` return ()
    numDistanceCodes <- (+ 1) <$> getAndConsume 5
    -- ("DistCodes: " ++ show numDistanceCodes) `trace` return ()
    numBitLengths <- (+ 4) <$> getAndConsume 4
    -- ("BitLengths: " ++ show numBitLengths) `trace` return ()
    let bitLengthPositions = Prelude.take numBitLengths border
    bitLengths' <- mapM (\blp -> (,) blp <$> getAndConsume 3) bitLengthPositions
    -- ("BitLengths: " ++ show bitLengths') `trace` return ()
    let bitLengths = makeFlexList (0, 18) 0 bitLengths'
        huft = makeHuffmanTree bitLengths 19 [] []
        entriesToRead = numLiterals + numDistanceCodes
    ll <- entryReader [] huft entriesToRead (-1)
    let lengthCodes = take numLiterals ll
        distCodes = take numDistanceCodes $ drop numLiterals ll
        lengthTree = makeHuffmanTree lengthCodes 257 cplens csExtraLenBits
        distTree = makeHuffmanTree distCodes 0 cpdist csExtraDistBits
    decodeIt lengthTree distTree

decompressStaticBlock :: State BitStream BS.ByteString
decompressStaticBlock =  do
    -- Length and dist codes copied from vpa108csulzh.cpp under GPL by SAP AG
    let lengthCodes = replicate 144 8 ++ replicate 112 9 ++ replicate 24 7 ++ replicate 8 8
        distCodes   = replicate 30 5
    -- End length and dist codes copied from vpa108csulzh.cpp under GPL by SAP AG
    let lengthTree  = makeHuffmanTree lengthCodes 257 cplens csExtraLenBits
        distTree    = makeHuffmanTree distCodes 0 cpdist csExtraDistBits
    decodeIt lengthTree distTree


