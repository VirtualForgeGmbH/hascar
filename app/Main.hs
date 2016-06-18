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
import Control.Monad.IO.Class
import Data.Binary.Get
import Path
import System.Directory
import System.Environment
import System.IO
import Text.Show.Pretty

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Options

import FlatedFile
import SAPCAR

import qualified CanonicalHuffmanTree as CHT

#ifndef mingw32_HOST_OS
import System.Posix.Files as SPF
#endif
import System.Posix.Types (CMode(..))

-- |Main entry point
main :: IO ()
main = runWithHeader it

it :: Options -> IO ()
it options = do
    withSapCarFile (oFilename options) $ do
        entries <- getEntries
        let files   = ((== CarFile) . cfFileType) `filter` entries
        let dirs    = ((== CarDirectory) . cfFileType) `filter` entries
        unless (oQuiet options) . liftIO . putStrLn $ show (length entries) ++ " entrie(s) in the archive."

        when (oListEntries options) $ liftIO $ do
            putStrLn "\nAll entries:"
            forM_ entries (putStrLn . ppShow)
            putStrLn ""

        when (oDecompress options) $ do
            forM_ dirs $ \dir -> do
                dirname <- parseRelDir $ T.unpack $ carEntryFilename dir
                when (oVerbose options) $
                    liftIO $ putStrLn $ "Creating " ++ show dirname
                liftIO $ do
                    createDirectoryIfMissing True $ fromRelDir dirname
#ifndef mingw32_HOST_OS
                    SPF.setFileMode (fromRelDir dirname) $ CMode $ cfPermissions dir
#endif

            forM_ files $ \file -> do
                filename <- parseRelFile $ T.unpack $ carEntryFilename file
                when (oVerbose options) $
                    liftIO $ putStrLn $ "Extracting " ++ show filename
                writeToFile file filename
#ifndef mingw32_HOST_OS
                liftIO $ SPF.setFileMode (fromRelFile filename) $ CMode $ cfPermissions file
#endif


