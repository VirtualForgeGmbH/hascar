-- |
-- Module: CanonicalHuffmanTree
-- Copyright: (C) 2015-2017, Virtual Forge GmbH
-- License: GPL2
-- Maintainer: Hans-Christian Esperer <hc@hcesperer.org>
-- Stability: experimental
-- Portability: portable
-- |
-- (De-)compress SAPCAR files

module Codec.Archive.SAPCAR.CanonicalHuffmanTree
    (CanonicalHuffmanTree, makeHuffmanTree,
     getEntry, CHTEntry, readEntry,
     litcode, eobcode,
     value, numBits, numExtraBits,
     isLitcode, isEobcode, readEntryRaw) where

import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.List
import           GHC.Arr

import           Codec.Archive.SAPCAR.BitStream
import           Codec.Archive.SAPCAR.FlexibleUtils

-- |A data structure representing a particular huffman tree entry
data CHTEntry = CHTEntry {
    -- |The value this entry encodes
      value        :: !Int
    -- |The "weight" of this entry
    , sortindex    :: !Int
    -- |The number of extra bits that need to be read to decode the value
    -- encoded partially by this huffman tree entry
    , numExtraBits :: !Int
    -- |The number of bits required to encode this huffman tree entry
    , numBits      :: !Int
    -- |The bits representing this huffman tree entry
    , bits         :: !Int
    } | CHTInvalid deriving (Eq, Show)

-- |A data structure representing a canonical huffman tree
data CanonicalHuffmanTree = CHT (GHC.Arr.Array Int CHTEntry) Int deriving (Show)

instance Ord CHTEntry where
    compare a b
        | numBits a < numBits b     = LT
        | numBits a > numBits b     = GT
        | sortindex a < sortindex b = LT
        | sortindex a > sortindex b = GT
        | otherwise                 = EQ

-- |Get a particular entry from a CanonicalHuffmanTree
getEntry :: CanonicalHuffmanTree -> Int -> CHTEntry
getEntry (CHT arry _) idx = arry ! idx

-- |Read one entry from a bitstream using the given
-- CanonicalHuffmanTree, returning the entry in the
-- huffman tree, not the value it encodes
readEntryRaw :: CanonicalHuffmanTree -> BitStream s -> ST s CHTEntry
readEntryRaw (CHT arry maxNumBits) stream = do
    bits' <- getBits stream maxNumBits
    let entry = arry ! bits'
    consume stream $ numBits entry
    return entry

-- |Read one entry from a bitstream using the given
-- CanonicalHuffmanTree
readEntry :: CanonicalHuffmanTree -> BitStream s -> ST s Int
readEntry cht s = value <$> readEntryRaw cht s

-- |A constant for literal entries
litcode :: Int
litcode = 16

-- |A constant meaning "end of bitstream"
eobcode :: Int
eobcode = 15

-- |Is the specified integer the special value
-- for "literal entries"?
isLitcode :: Int -> Bool
isLitcode = (== litcode)

-- |Is the specified integer the special value
-- indicating "end of bitstream"?
isEobcode :: Int -> Bool
isEobcode = (== eobcode)

-- |Make a huffman tree. Takes a list of the bit lengths, the number of
-- "regular" entries (integer values counting from zero), plus a list of
-- additional entries and a list of extra lengths
makeHuffmanTree :: [Int] -> Int -> [Int] -> [Int] -> CanonicalHuffmanTree
makeHuffmanTree bitLengths countRegular extraBits' extraLengths = CHT chtArray maximumLength
  where
    chtArray = makeFlexArray (0, maximum . map fst $ chtentries''') CHTInvalid chtentries'''
    chtentries = sort . map mkChtEntry . enumerate $ zip3 values extraBits bitLengths
    chtentries' = filter ((/= 0) . numBits) chtentries
    (_, _, chtentries'') = foldl assignBitValue (-1, 1, []) chtentries'
    chtentries''' = concatMap fillEmUp chtentries''
    fillEmUp entry' = map (fillIt entry') [0..max' - 1]
      where
        max' = shiftL 1 padBits
        padBits = maximumLength - numBits entry'
        entryVal = reverseBits (bits entry') (numBits entry')
        fillIt entry i = (entryVal + i `shiftL` numBits entry, entry)
    assignBitValue (lastbitval, lastbitlen, entries) entry =
        (bitval, bitlen, newentry:entries)
      where
        bitval
            | bitlen > lastbitlen && lastbitval == -1 = 0
            | bitlen > lastbitlen                     = inc `shiftL` incBy
            | otherwise                               = lastbitval + 1
        inc = lastbitval + 1
        incBy = bitlen - lastbitlen
        bitlen = numBits entry
        newentry = entry { bits=bitval }
    mkChtEntry :: (Int, (Int, Int, Int)) -> CHTEntry
    mkChtEntry (i, (value', numextraBits, bitLength)) = CHTEntry {
        value=value',
        sortindex=i,
        numExtraBits=numextraBits,
        numBits=bitLength,
        bits= -1}
    maximumLength = maximum bitLengths
    (values, extraBits) = unzip pairs
    pairs = map genValues . enumerate $ regulars ++ zip extraBits' extraLengths
    enumerate = zip [0..]
    regulars = enumerate [0..countRegular - 1]
    genValues (i,(j,k))
        | i < countRegular && i < 256 = (i, litcode)
        | i < countRegular            = (i, eobcode)
        | otherwise                   = (j, k)

reverseBits :: Int -> Int -> Int
reverseBits x numBits' = result
  where
    (_, result) = foldl step (x, 0) [1..numBits']
    step (y, s) _ = (newY, newS)
      where
        newY = y `shiftR` 1
        newS
          | lowestY == 1 = s `shiftL` 1 + 1
          | otherwise    = s `shiftL` 1
        lowestY = y .&. 1

