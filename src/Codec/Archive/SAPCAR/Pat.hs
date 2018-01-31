-- |
-- Module: Pat
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
module Codec.Archive.SAPCAR.Pat
    ( patToTransport
    ) where

import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.Conduit
import Data.Maybe
import Data.Word
import Data.Text (Text)
import System.IO
import Text.Printf
import Text.Read

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

-- | The maximum size we allow per chunk. This is
-- important to help prevent memory exhaustion attacks.
maximumChunkSize :: Int
maximumChunkSize = 65536

-- | One chunk in a PAT file
data PatChunk = PatChunk
    { -- | The version of the chunk
      pcVersion         :: Int
    , -- | The type of the PAT chunk
      pcType            :: PatChunkType
    , -- | The length ot the chunk
      pcLength          :: Int
    , -- | Reserved 14 bytes
      pcReserved        :: S.ByteString
    , -- | The raw payload of the chunk
      pcPayload         :: S.ByteString }
    deriving (Show, Eq)

-- | The type of a PAT chunk
data PatChunkType
    = -- | A fragment of an SAP transport file
      TransportPatChunk
    | -- | An unknown type of chunk
      UnknownPatChunk
    | -- | Not a PAT fiel
      NotAPatFile
    deriving (Eq, Enum, Show)

getChunkType :: Get PatChunkType
getChunkType = getChunkType' <$> getWord8

getChunkType' :: Word8 -> PatChunkType
getChunkType' 82   = TransportPatChunk
getChunkType' (-1) = NotAPatFile
getChunkType' _    = UnknownPatChunk

getChunkVersion :: Get Int
getChunkVersion = fromMaybe (-1) . readMaybe . T.unpack . TE.decodeUtf8With TEE.lenientDecode <$> getByteString 2

getChunkLength :: Get Int
getChunkLength = do
    length <- fromMaybe (0) . readMaybe . T.unpack . TE.decodeUtf8 <$> getByteString 8 :: Get Int
    when (length > maximumChunkSize) $ error "Too big a chunk"
    return length

-- | Get one SAP PAT chunk
getPatChunk :: Get PatChunk
getPatChunk = do
    v <- getChunkVersion
    if v == (-1)
    then return $ PatChunk (-1) NotAPatFile 0 S.empty S.empty
    else do
        t <- getChunkType
        l <- getChunkLength
        r <- getByteString 14
        p <- getByteString $ l - 25
        return $ PatChunk v t l r p

-- | Extract a transport file from a PAT file chunk by chunk
patToTransport :: Monad m => Conduit S.ByteString m S.ByteString
patToTransport = patToTransport' S.empty $ runGetIncremental getPatChunk

patToTransport'
    :: Monad m
    => S.ByteString
    -> Decoder PatChunk
    -> Conduit S.ByteString m S.ByteString
patToTransport' s (Partial d)
    | S.null s = do
        chunk <- await
        case chunk of
            Just chunk' -> patToTransport' S.empty $ pushChunk (Partial d) chunk'
            Nothing     -> return ()
    | otherwise = patToTransport' S.empty $ pushChunk (Partial d) s
patToTransport' s (Done rest _ r) = do
    when (pcType r == TransportPatChunk) $
        yield (pcPayload r)
    unless (pcType r == NotAPatFile) $
        patToTransport' rest $ runGetIncremental getPatChunk

