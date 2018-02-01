{-# LANGUAGE OverloadedStrings, DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification, ImpredicativeTypes #-}
-- |
-- Module: SAPCAR
-- Copyright: (C) 2015-2017, Virtual Forge GmbH
-- License: GPL2
-- Maintainer: Hans-Christian Esperer <hc@hcesperer.org>
-- Stability: experimental
-- Portability: portable
-- |
-- (De-)compress SAPCAR files

module Codec.Archive.SAPCAR
    ( SapCar
    , CarEntry (..)
    , CarFileType (..)
    , carEntryFilename
    , withSapCarFile
    , withSapCarPath
    , withSapCarHandle
    , getEntries
    , sourceEntry
    , writeToFile
    , writeToHandle
    ) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Catch
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Conduit
import Data.Int
import Data.Word
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Path
import System.IO
import Text.Printf

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Codec.Archive.SAPCAR.FlatedFile as FF

-- | The SAPCAR monad. All operations on SAPCAR files
-- should happen inside this monad.
newtype SapCar s m a = SapCar
 { unSapCar :: StateT SapCarFile m a }
 deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask )

-- | The state during operations inside the SAPCAR monad.
data SapCarFile = SapCarFile
    { -- | The handle to the open SAPCAR file
      sarFileH              :: !Handle
    }

-- | The SAPCAR file header
data SapCarHeader s = SapCarHeader
    { -- | The version string of the SAPCAR archive
      scVersion             :: !Text
    , -- | Meta information on all files and directories in an archive
      scFiles               :: ![CarEntry s]
    } deriving (Show)

-- | The type of an entry in the SAPCAR file
data CarFileType
    = -- | A regular file
      CarFile
    | -- | A directory
      CarDirectory
    | -- | Something else
      CarUnknown
    deriving (Show, Eq, Enum)

-- | Get the type of an entry in the SAPCAR file
getType :: Get CarFileType
getType = getType' <$> getByteString 2
    where
        getType' t
            | t == "RG" = CarFile
            | t == "DR" = CarDirectory
            | otherwise = CarUnknown

-- | Meta information about a single file or directory
-- in a SAPCAR archive
data CarEntry s = CarEntry
    { -- | The type of the entry
      cfFileType            :: !CarFileType
    , -- | The unix style permissions of the entry
      cfPermissions         :: !Word32
    , -- | The uncompressed length of the whole file, if it is a file
      cfLength              :: !Word32
    , -- | The EPOCH timestamp of the file
      cfTimestamp           :: !Word32
    , -- | The filename
      cfFileName            :: !Text
    , -- | The absolute offset of the entry in the SAPCAR file
      cfFileOffset          :: !Int64
    , -- | The absolute offset of the payload of the entry
      cfPayloadOffset       :: !Int64
    }

instance Show (CarEntry s) where
    show ce = printf "%s%s 0 root root %d\t%s 00:00 %s"
            (case cfFileType ce of
                CarFile         -> "-" :: Text
                CarDirectory    -> "d"
                CarUnknown      -> "X")
            (toPermissionText $ cfPermissions ce)
            (cfLength ce)
            (unparseDate $ cfTimestamp ce)
            (cfFileName ce)

-- | Convert an EPOCH date to a string
-- that is compatible to the output of the UNIX(R)
-- "ls -l" command
unparseDate :: Word32 -> String
unparseDate = formatTime defaultTimeLocale "%b %e" . posixSecondsToUTCTime . fromIntegral

-- | Convert UNIX permissions to a string
-- that is compatible to the output of the UNIX(R)
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

-- | Get the filename of a car entry
carEntryFilename :: CarEntry s -> Text
carEntryFilename = cfFileName

-- | The compression algorithm used
data CompAlg
    = -- | Lempel-Ziv huffman
      CompLzh
    | -- | LZC
      CompLzc
    | -- | Something else
      CompUnknown
    deriving (Show, Eq, Enum)

-- | The header of one compressed SAPCAR block.
-- This is not to be confused with a single compressed
-- block! One SAPCAR block usually contains one or two
-- lzh blocks. Yes, it's confusing and not yielding the
-- best compression ratio...
data CompHdr = CompHdr
    { -- | The length.
      chLen                 :: !Word32
    , -- | The used algorithm. This is indeed encoded for each block.
      chAlg                 :: !CompAlg
    , -- | The magic number.
      chMagic               :: !Word16
    , -- | The special byte. Meaning depends on the CompAlg.
      chSpe                 :: !Word8
    } deriving (Show)


-- | Run all actions in the SapCar monad.
withSapCarPath
    :: (MonadIO m, MonadThrow m, MonadMask m)
    => Path b File
    -> (forall s. SapCar s m a)
    -> m a
withSapCarPath sarfile a = bracket open close $ withSapCarHandle a
    where
        open   = liftIO $ openBinaryFile (toFilePath sarfile) ReadMode
        close  = liftIO . hClose

-- | Run all actions in the SapCar monad.
withSapCarFile
    :: (MonadIO m, MonadThrow m, MonadMask m)
    => FilePath
    -> (forall s. SapCar s m a)
    -> m a
withSapCarFile sarfile a = bracket open close $ withSapCarHandle a
    where
        open   = liftIO $ openBinaryFile sarfile ReadMode
        close  = liftIO . hClose

-- | Run all actions in the SapCar monad.
withSapCarHandle
    :: (MonadIO m, MonadThrow m, MonadMask m)
    => (forall s. SapCar s m a)
    -> Handle
    -> m a
withSapCarHandle a = evalStateT (unSapCar a) . SapCarFile

--     let res = runGet (parseFileHdr >> parseSAPCARFile []) c

-- | Get all entries contained inside the SapCar file.
getEntries :: MonadIO m => SapCar s m [CarEntry s]
getEntries = SapCar $ do
    fh <- sarFileH <$> get
    let entryParser = runGetIncremental (parseFileHdr >> parseSAPCARFile [])
    res <- liftIO $ feedChunks entryParser fh
    let Done _ _ entries = res
    return entries

-- | Stream the contents of the given SapCar entry to
-- the specified conduit sink.
sourceEntry :: MonadIO m => CarEntry s -> Sink S.ByteString IO a -> SapCar s m a
sourceEntry entry sink = SapCar $ do
    fh <- sarFileH <$> get
    case cfLength entry of
        0 -> liftIO $ emptySource $$ sink
        _ -> do
            liftIO $ hSeek fh AbsoluteSeek $ fromIntegral $ cfPayloadOffset entry
            liftIO $ decompressBlocks fh $$ sink

emptySource :: Source IO S.ByteString
emptySource = yield ""

-- | Feed chunks of data to the Get monad
feedChunks :: Decoder a -> Handle -> IO (Decoder a)
feedChunks d h = do
    chunk <- S.hGet h 8192
    if chunk == S.empty
    then return $ pushEndOfInput d
    else feedChunks (pushChunk d chunk) h

-- | Parse all SAPCAR entries. (tail recursive)
parseSAPCARFile :: [CarEntry s] -> Get [CarEntry s]
parseSAPCARFile acc = do
    empty <- isEmpty
    if empty
    then return acc
    else do
        entry <- parseEntry
        parseSAPCARFile $ entry:acc

-- | Write a SapCar entry to the specified file.
writeToFile :: (MonadIO m, MonadMask m, MonadThrow m) => CarEntry s -> Path b File -> SapCar s m ()
writeToFile entry path = bracket open close w
    where
        open    = liftIO $ openBinaryFile (toFilePath path) WriteMode
        close   = liftIO . hClose
        w       = sourceEntry entry . writer

-- | Write a SapCar entry to the specified handle.
writeToHandle :: (MonadIO m, MonadMask m, MonadThrow m) => CarEntry s -> Handle -> SapCar s m ()
writeToHandle entry = sourceEntry entry . writer

-- | Provide a conduit sink, write everything that arrives there to the given handle.
writer :: Handle -> Sink S.ByteString IO ()
writer h = do
    chunk <- await
    case chunk of
        Just chunk' -> liftIO (S.hPut h chunk') >> writer h
        Nothing -> return ()

-- | Parse the compression header of one SAPCAR block.
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

-- | Parse one SAPCAR entry's metadata, ignoring its contents.
parseEntry :: Get (CarEntry s)
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
    case ftype of
        CarFile -> do
            payloadOffset <- bytesRead
            unless (flen == 0) skipBlocks
            return $ CarEntry ftype fperm flen ftimestamp (TE.decodeUtf8 fn) fileOffset payloadOffset
        CarDirectory ->
            return $ CarEntry ftype fperm flen ftimestamp (TE.decodeUtf8 fn) fileOffset 0
        _ -> error $ "Unhandled type " ++ show ftype

-- | Skip all SAPCAR payload blocks for one SAPCAR entry.
skipBlocks :: Get ()
skipBlocks = do
    ed <- getByteString 2
    skipBlock
    case ed of
        "ED" -> void getWord32le
        "UE" -> void getWord32le
        "DA" -> skipBlocks
        "UD" -> skipBlocks
        _    -> error $ "Unknown block type " ++ show ed

-- | Skip one SAPCAR payload block.
skipBlock :: Get ()
skipBlock = void (getWord32le >>= getByteString . fromIntegral)

-- | Decompress all SAPCAR blocks of one SAPCAR entry.
decompressBlocks :: Handle -> Source IO S.ByteString
decompressBlocks h = do
    ed <- liftIO $ S.hGet h 2
    case ed of
        -- Compressed block (any algorithm; last block)
        "ED" -> do
            liftIO (decompressBlock h) >>= yield
            void $ liftIO $ S.hGet h 4 -- TODO: This is the crc value. Use it!

        -- Compressed block (any algorithm; more to follow)
        "DA" -> do
            liftIO (decompressBlock h) >>= yield
            decompressBlocks h

        -- Uncompressed block (more to follow)
        "UD" -> do
            liftIO (uncompressedBlock h) >>= yield
            decompressBlocks h

        -- Uncompressed block (last block)
        -- Looks like uncompressed files don't have a CRC appended
        "UE" -> liftIO (uncompressedBlock h) >>= yield

        _    -> error $ "(while decompressing) unknown block type " ++ show ed

-- | Handle one SAPCAR block that is stored uncompressed
uncompressedBlock :: Handle -> IO S.ByteString
uncompressedBlock h = do
    blockSize <- S.hGet h 4
    let blockSize' = runGet getWord32le $ L.fromStrict blockSize
    S.hGet h $ fromIntegral blockSize'

-- | Handle one SAPCAR block that consists of
-- *one or more* compressed blocks of any supported
-- compression algorithm.
decompressBlock :: Handle -> IO S.ByteString
decompressBlock h = do
    hdr <- L.fromStrict <$> S.hGet h 12
    let (fCompLen, compHdr) = runGet ((,) <$> getWord32le <*> parseCompHdr) hdr
    when (chAlg compHdr /= CompLzh) $ error "Currently only LZH is supported, not LZC"
    blob <- S.hGet h $ fromIntegral fCompLen - 8
    when (chLen compHdr > 655360) $ error "Max 640k block size supported!"
    return $ FF.decompressBlocks (fromIntegral $ chLen compHdr) blob

-- | Parse (ignore, for now) the SAPCAR global header
parseFileHdr :: Get ()
parseFileHdr = do
    hdr <- getByteString 8
    unless (hdr == "CAR 2.01") $ error "Only the newest SAPCAR format (2.01) is supported"

