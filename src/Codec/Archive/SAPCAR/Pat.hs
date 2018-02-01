-- |
-- Module: Pat
-- Copyright: (C) 2015-2018, Virtual Forge GmbH
-- License: GPL2
-- Maintainer: Hans-Christian Esperer <hc@hcesperer.org>
-- Stability: experimental
-- Portability: portable
-- |
-- (De-)compress SAPCAR files

module Codec.Archive.SAPCAR.Pat
    ( patToTransport
    , getPatHeader
    , PatHeader(..)
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
      pcVersion         :: !Int
    , -- | The type of the PAT chunk
      pcType            :: !PatChunkType
    , -- | The length ot the chunk
      pcLength          :: !Int
    , -- | Reserved 14 bytes
      pcReserved        :: !S.ByteString
    , -- | The raw payload of the chunk
      pcPayload         :: !S.ByteString }
    deriving (Show, Eq)

-- | The type of a PAT chunk
data PatChunkType
    = -- | The primary header of the PAT file
      PatPrimaryHeader
    | -- | A fragment of an SAP transport file
      TransportPatChunk
    | -- | An unknown type of chunk
      UnknownPatChunk
    | -- | Not a PAT fiel
      NotAPatFile
    deriving (Eq, Enum, Show)

data PatHeader = PatHeader
    { phMagic           :: !S.ByteString -- 3 bytes
    , phTransportName   :: !S.ByteString -- 20 bytes
    , phTitle           :: !S.ByteString -- 60 bytes
    } deriving (Eq, Show)

readPatPrimaryHeader :: Get PatHeader
readPatPrimaryHeader = PatHeader <$> getByteString 3 <*> getByteString 20 <*> getByteString 60

getChunkType :: Get PatChunkType
getChunkType = getChunkType' <$> getWord8

getChunkType' :: Word8 -> PatChunkType
getChunkType' 65   = PatPrimaryHeader
getChunkType' 82   = TransportPatChunk
getChunkType' (-1) = NotAPatFile
-- getChunkType' x    = show x `trace` UnknownPatChunk
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

getPatHeader :: Monad m => Conduit S.ByteString m PatHeader
getPatHeader = getPatHeader' S.empty $ runGetIncremental getPatChunk

getPatHeader'
    :: Monad m
    => S.ByteString
    -> Decoder PatChunk
    -> Conduit S.ByteString m PatHeader
getPatHeader' s (Partial d)
    | S.null s = do
        chunk <- await
        case chunk of
            Just chunk' -> getPatHeader' S.empty $ pushChunk (Partial d) chunk'
            Nothing     -> return ()
    | otherwise = getPatHeader' S.empty $ pushChunk (Partial d) s
getPatHeader' s (Done rest _ r) = do
    when (pcType r == PatPrimaryHeader) $
        yield $ runGet readPatPrimaryHeader $ L.fromStrict $ pcPayload r
    unless (pcType r == NotAPatFile) $
        getPatHeader' rest $ runGetIncremental getPatChunk


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

