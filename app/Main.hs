{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Main
-- Copyright: (C) 2015-2018, Virtual Forge GmbH
-- License: GPL2
-- Maintainer: Hans-Christian Esperer <hc@hcesperer.org>
-- Stability: experimental
-- Portability: portable
-- |
-- (De-)compress SAPCAR files

module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.Conduit
import Foreign.C.Types (CTime(..))
import Path
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.List as DCL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Options

import Codec.Archive.SAPCAR
import Codec.Archive.SAPCAR.Pat

#ifndef mingw32_HOST_OS
import System.Posix.Files as SPF
#endif
import System.Posix.Types (CMode(..), EpochTime(..))

-- |Main entry point
main :: IO ()
main = run it

it :: Options -> IO ()
it options = withSapCarFile (oFilename options) $ do
    entries <- getEntries
    let files   = ((== CarFile) . cfFileType) `filter` entries
    let dirs    = ((== CarDirectory) . cfFileType) `filter` entries
    unless (oQuiet options) . liftIO . putStrLn $ show (length entries) ++ " entrie(s) in the archive."

    when (oListEntries options) $ liftIO $ do
        putStrLn "\nAll entries:"
        forM_ entries print
        putStrLn ""

    liftIO $ case oExtractDir options of
        Just extractDir -> do
            when (oVerbose options) $
                putStrLn $ "Setting cwd to " ++ show extractDir
            setCurrentDirectory extractDir
        Nothing -> return ()

    when (oDecompress options) $ do
        unless (oExtractPatFiles options) $ do
            forM_ dirs $ \dir -> do
                dirname <- parseRelDir $ T.unpack $ carEntryFilename dir
                when (oVerbose options) $
                    liftIO $ putStrLn $ "Creating " ++ show dirname
                liftIO $ do
                    createDirectoryIfMissing True $ fromRelDir dirname
#ifndef mingw32_HOST_OS
                    SPF.setFileMode (fromRelDir dirname) $ CMode $
                        fromIntegral $ cfPermissions dir
                    let amTime = CTime $ fromIntegral $ cfTimestamp dir
                    SPF.setFileTimes (fromRelDir dirname) amTime amTime
#endif

        forM_ files $ \file -> do
            filename <- parseRelFile $ T.unpack $ carEntryFilename file
            unless (oExtractPatFiles options) $ void $ liftIO $ cdim $ fromRelFile filename
            patWritten <- if (oExtractPatFiles options)
            then do
                patInfo <- loadPatInfo file
                case patInfo of
                    Just patInfo' -> do
                        let transportName = T.unpack . T.strip . TE.decodeUtf8With TEE.lenientDecode . phTransportName $ patInfo'
                            patFilename = "R" ++ drop 4 transportName
                            patExt = take 3 transportName
                            transportFilename = patFilename ++ "." ++ patExt
                        liftIO $ when (oVerbose options) $ do
                            putStrLn "Transport file"
                            putStrLn "==============================================================================="
                            putStrLn $ "Transport file : " ++ show (phTransportName patInfo')
                            putStrLn $ "Title          : " ++ show (phTitle patInfo')
                            putStrLn $ "Extracting to  : " ++ show transportFilename
                            putStrLn "\n"
                        parsedTransportFilename <- parseRelFile transportFilename
                        unpackPat (fromRelFile parsedTransportFilename) file
                    Nothing -> return True
            else return False
            if patWritten
            then return () -- do nothing here
            else do
                when (oVerbose options) $
                    liftIO $ putStrLn $ "x " ++ fromRelFile filename
                writeToFile file filename
#ifndef mingw32_HOST_OS
                liftIO $ SPF.setFileMode (fromRelFile filename) $ CMode $
                    fromIntegral $ cfPermissions file
                let amTime = CTime $ fromIntegral $ cfTimestamp file
                liftIO $ SPF.setFileTimes (fromRelFile filename) amTime amTime
#endif

-- | Write a transport contained inside a PAT file
-- return if any bytes were written
unpackPat :: FilePath -> CarEntry s -> SapCar s IO Bool
unpackPat path file = bracket open close w
    where
        open    = liftIO $ openBinaryFile path WriteMode
        close   = liftIO . hClose
        w h     = sourceEntry file (patToTransport =$= writePat h)

loadPatInfo :: CarEntry s -> SapCar s IO (Maybe PatHeader)
loadPatInfo file = sourceEntry file (getPatHeader =$= DCL.head)

-- | Provide a conduit sink, write everything that arrives there to
-- the given handle. Return True if at least one chunk was written.
writePat :: Handle -> Sink S.ByteString IO Bool
writePat h =
    let loop c = do
            chunk <- await
            case chunk of
                Just chunk' -> liftIO (S.hPut h chunk') >> loop True
                Nothing -> return c
    in loop False

cdim :: FilePath -> IO ()
cdim fp = do
    let fpt     = T.pack fp
        parts   = T.split (== pathSeparator) fpt

    when (length parts > 1) $ do
        let path = T.unpack $ T.intercalate (T.pack [pathSeparator]) $ take (length parts - 1) parts
        p <- parseRelDir path
        createDirectoryIfMissing True $ fromRelDir p

