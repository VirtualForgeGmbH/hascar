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


data Options = Options
    { oFilename             :: !FilePath
    , oDecompress           :: !Bool
    , oVerbose              :: !Bool
    } deriving (Show)


main :: IO ()
-- main = CHT.main -- printGpl >> putStrLn "" >> execParser spec >>= doit
main = printGpl >> putStrLn "" >> execParser spec >>= doit

doit options = do
    f <- parseAbsFile $ oFilename options
    withSapCarFile f $ do
        files <- getEntries
        liftIO $ do
            putStrLn "\nAll files:"
            forM_ files $ \file -> putStrLn $ ppShow file
            putStrLn ""
            unless (oDecompress options) $ putStrLn "Use -x switch to extract archive"
        when (oDecompress options) $
            forM_ files $ \file -> do
                path <- parseRelFile $ T.unpack $ carEntryFilename file
                liftIO $ putStrLn $ "Extracting " ++ show path
                writeToFile file path

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

