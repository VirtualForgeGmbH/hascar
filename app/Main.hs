{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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

    when (oDecompress options) $ do
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
            liftIO $ cdim $ fromRelFile filename
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
                            putStrLn $ "Title          : " ++ (T.unpack . T.strip . T.pack . show . phTitle $ patInfo')
                            putStrLn $ "Extracting to  : " ++ transportFilename
                            putStrLn "\n"
                        parsedTransportFilename <- parseRelFile transportFilename
                        unpackPat (fromRelFile parsedTransportFilename) file
                    Nothing -> return False
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

