{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
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
module SAPCAR
    ( SapCar
    , CarEntry
    , carEntryFilename
    , withSapCarFile
    , withSapCarHandle
    , getEntries
    , sourceEntry
    , writeToFile
    ) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Catch
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Conduit
import Data.Int
import Data.Knob
import Data.Word
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Path
import System.IO
import Text.Printf
import Text.Show.Pretty

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified FlatedFile as FF

import Debug.Trace

type SapCar m a = StateT SapCarFile m a

data SapCarFile = SapCarFile
    { sarFileH              :: Handle
    }

data SapCarHeader = SapCarHeader
    { scVersion             :: !Text
    , scFiles               :: ![CarEntry]
    } deriving (Show)

data CarFileType
    = CarFile
    | CarDirectory
    | CarUnknown
    deriving (Show, Eq, Enum)

getType :: Get CarFileType
getType = getType' <$> getByteString 2
    where
        getType' t
            | t == "RG" = CarFile
            | t == "DR" = CarDirectory
            | otherwise = CarUnknown

data CarEntry = CarEntry
    { cfFileType            :: !CarFileType
    , cfPermissions         :: !Word32
    , cfLength              :: !Word32
    , cfTimestamp           :: !Word32
    , cfFileName            :: !Text
    , cfFileOffset          :: !Int64
    , cfPayloadOffset       :: !Int64
    }

instance Show CarEntry where
    show ce = printf "%s%s 0 root root %d\t%s 00:00 %s"
            (case cfFileType ce of
                CarFile         -> "-" :: Text
                CarDirectory    -> "d"
                CarUnknown      -> "X")
            (toPermissionText $ cfPermissions ce)
            (cfLength ce)
            (unparseDate $ cfTimestamp ce)
            (cfFileName ce)

unparseDate :: Word32 -> String
unparseDate = formatTime defaultTimeLocale "%b %e" . posixSecondsToUTCTime . fromIntegral

toPermissionText :: Word32 -> Text
toPermissionText n = T.concat [u, g, o]
    where
        u = toPermissionText' $ n `shiftR` 6 .&. 7
        g = toPermissionText' $ n `shiftR` 3 .&. 7
        o = toPermissionText' $ n .&. 7

toPermissionText' :: Word32 -> Text
toPermissionText' n = T.concat [r `perm` "r", w `perm` "w", x `perm` "x"]
    where
        x = n .&. 1 == 1
        w = n `shiftR` 1 .&. 1 == 1
        r = n `shiftR` 2 .&. 1 == 1

perm :: Bool -> Text -> Text
perm True w = w
perm False w = "-"

carEntryFilename :: CarEntry -> Text
carEntryFilename = cfFileName

data CompAlg
    = CompLzh
    | CompLzc
    | CompUnknown
    deriving (Show, Eq, Enum)

data CompHdr = CompHdr
    { chLen                 :: !Word32
    , chAlg                 :: !CompAlg
    , chMagic               :: !Word16
    , chSpe                 :: !Word8
    } deriving (Show)


withSapCarFile
    :: (MonadIO m, MonadThrow m, MonadMask m)
    => Path b File
    -> SapCar m a
    -> m a
withSapCarFile sarfile = bracket open close . withSapCarHandle
    where
        open   = liftIO $ openBinaryFile (toFilePath sarfile) ReadMode
        close  = liftIO . hClose

withSapCarHandle
    :: (MonadIO m, MonadThrow m, MonadMask m)
    => SapCar m a
    -> Handle
    -> m a
withSapCarHandle a = evalStateT a . SapCarFile

--     let res = runGet (parseFileHdr >> parseSAPCARFile []) c

getEntries :: MonadIO m => SapCar m [CarEntry]
getEntries = do
    fh <- sarFileH <$> get
    let entryParser = runGetIncremental (parseFileHdr >> parseSAPCARFile [])
    res <- liftIO $ feedChunks entryParser fh
    let Done _ _ entries = res
    return entries

sourceEntry :: MonadIO m => CarEntry -> Sink S.ByteString IO a -> SapCar m a
sourceEntry entry sink = do
    fh <- sarFileH <$> get
    liftIO $ hSeek fh AbsoluteSeek $ fromIntegral $ cfPayloadOffset entry
    liftIO $ decompressBlocks fh $$ sink


feedChunks :: Decoder a -> Handle -> IO (Decoder a)
feedChunks d h = do
    chunk <- S.hGet h 8192
    if chunk == S.empty
    then return $ pushEndOfInput d
    else feedChunks (pushChunk d chunk) h


parseSAPCARFile :: [CarEntry] -> Get [CarEntry]
parseSAPCARFile acc = do
    empty <- isEmpty
    if empty
    then return acc
    else do
        entry <- parseEntry
        parseSAPCARFile $ entry:acc

writeToFile :: (MonadIO m, MonadMask m, MonadThrow m) => CarEntry -> Path b File -> SapCar m ()
writeToFile entry path = bracket open close w
    where
        open    = liftIO $ openBinaryFile (toFilePath path) WriteMode
        close   = liftIO . hClose
        w       = sourceEntry entry . writer

writer :: Handle -> Sink S.ByteString IO ()
writer h = do
    chunk <- await
    case chunk of
        Just chunk' -> do
            liftIO $ do
                -- S.hPut h "================== BEGIN CHUNK =======================================\n"
                S.hPut h chunk'
                -- S.hPut h "\n============================ END CHUNK ===============================\n"
            writer h
        Nothing -> return ()

parseCompHdr :: Get CompHdr
parseCompHdr = do
    len <- getWord32le
    alg <- getWord8
    let alg' = case alg of
            18 -> CompLzh
            16 -> CompLzc
            _  -> CompUnknown
    magic <- getWord16be
    when (magic /= 8093) $ error $ "Invalid magic value (8093 decimal expected); got " ++ show magic
    spe <- getWord8
    return $ CompHdr len alg' magic spe

parseEntry :: Get CarEntry
parseEntry = do
    fileOffset <- bytesRead
    ftype <- getType
    fperm <- getWord32le
    flen <- getWord32le
    void $ getByteString 8
    ftimestamp <- getWord32le
    void $ getByteString 10
    fnlen <- fromIntegral <$> getWord16le
    fn <- getByteString $ fnlen - 1
    nulbyte <- getWord8
    when (nulbyte /= 0) $ error "NUL byte expected"
    payloadOffset <- bytesRead
    skipBlocks
    return $ CarEntry ftype fperm flen ftimestamp (TE.decodeUtf8 fn) fileOffset payloadOffset

skipBlocks :: Get ()
skipBlocks = do
    ed <- getByteString 2
    skipBlock
    case ed of
        "ED" -> getWord32le >> return ()
        "UE" -> getWord32le >> return ()
        "DA" -> skipBlocks
        "UD" -> skipBlocks
        _    -> error $ "Unknown block type " ++ show ed

skipBlock :: Get ()
skipBlock = void (getWord32le >>= getByteString . fromIntegral)

--    blocks <- reverse <$> decompressBlocks []

decompressBlocks :: Handle -> Source IO S.ByteString
decompressBlocks h = do
    ed <- liftIO $ S.hGet h 2
    case ed of
        "ED" -> do
            (liftIO $ decompressBlock h) >>= yield
            void $ liftIO $ S.hGet h 4 -- TODO: This is the crc value. Use it!
        "DA" -> do
            (liftIO $ decompressBlock h) >>= yield
            decompressBlocks h
        "UD" -> do
            (liftIO $ uncompressedBlock h) >>= yield
            decompressBlocks h
        "UE" -> (liftIO $ uncompressedBlock h) >>= yield
        _    -> error $ "Unknown block type " ++ show ed

uncompressedBlock :: Handle -> IO S.ByteString
uncompressedBlock h = do
    blockSize <- S.hGet h 4
    let blockSize' = runGet getWord32le $ L.fromStrict blockSize
    S.hGet h $ fromIntegral blockSize'

decompressBlock :: Handle -> IO S.ByteString
decompressBlock h = do
    hdr <- L.fromStrict <$> S.hGet h 12
    let (fCompLen, compHdr) = runGet ((,) <$> getWord32le <*> parseCompHdr) hdr
    when (chAlg compHdr /= CompLzh) $ error "Currently only LZH is supported, not LZC"
    blob <- S.hGet h $ fromIntegral fCompLen - 8
    return $ FF.decompressBlock blob

parseFileHdr :: Get ()
parseFileHdr = void $ getByteString 8

