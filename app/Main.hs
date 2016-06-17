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
import Options.Applicative
import Path
import System.Directory
import System.Environment
import System.IO
import Text.Show.Pretty

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import FlatedFile
import GPL
import SAPCAR

import qualified CanonicalHuffmanTree as CHT

import System.Posix.Files as SPF
import System.Posix.Types (CMode(..))

-- |HASCAR runtime options
data Options = Options
    { -- | Filename of the SAPCAR archive
      oFilename             :: !FilePath
    , -- | Whether to extract the archive's contents
      oDecompress           :: !Bool
    , -- | Whether to print verbose information during operation
      oVerbose              :: !Bool
    , -- | Whether to suppress printing the GPL header and other info
      oQuiet                :: !Bool
    , -- | Whether to list all entries in the archive
      oListEntries          :: !Bool
    } deriving (Show)

-- |Main entry point
main :: IO ()
main = execParser spec >>= doit

doit options = do
    unless (oQuiet options) (printGpl >> putStrLn "")
    f <- parseAbsFile $ oFilename options
    withSapCarFile f $ do
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
                    SPF.setFileMode (fromRelDir dirname) $ CMode $ cfPermissions dir

            forM_ files $ \file -> do
                filename <- parseRelFile $ T.unpack $ carEntryFilename file
                when (oVerbose options) $
                    liftIO $ putStrLn $ "Extracting " ++ show filename
                writeToFile file filename
                liftIO $ SPF.setFileMode (fromRelFile filename) $ CMode $ cfPermissions file

spec = info (helper <*> optionsParser)
     (  fullDesc
     <> progDesc "Decompress SAPCAR archives"
     <> header "sapcar extractor" )

optionsParser :: Parser Options
optionsParser = Options
    <$> option str
        (  metavar "SAPCARFILE"
        <> short 'f'
        <> long "file"
        <> help "Path to the SAPCAR file" )
    <*> switch
        (  long "extract"
        <> short 'x'
        <> help "Extract archive contents into current directory" )
    <*> switch
        (  long "verbose"
        <> short 'v'
        <> help "Verbose operation" )
    <*> switch
        (  long "quiet"
        <> short 'q'
        <> help "Do not print the header" )
    <*> switch
        (  long "list"
        <> short 't'
        <> help "List all entries in the archive" )


