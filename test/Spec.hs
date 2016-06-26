{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Crypto.Hash.SHA256
import Data.Conduit
import Data.Hex
import Path
import System.IO

import qualified Data.ByteString as S

import Codec.Archive.SAPCAR

bufSink :: Sink S.ByteString IO S.ByteString
bufSink = S.concat . reverse <$> bufSink' []
    where
        bufSink' :: [S.ByteString] -> Sink S.ByteString IO [S.ByteString]
        bufSink' acc = do
            chunk <- await
            case chunk of
                Just chunk' -> bufSink' (chunk':acc)
                Nothing     -> return acc

main :: IO ()
main = do
    path <- parseRelFile "test/test6.sar"
    res <- withSapCarPath path $ do
        entries <- getEntries
        unless (length entries == 1) $ error "test6.sar should contain exactly one entry!"
        let entry = head entries
        unless (cfFileName entry == "pg244.txt") $ error "Entry in test6.sar should be called 'pg244.txt'!"
        unless (cfFileType entry == CarFile) $ error "Entry in test6.sar should be of type 'file'!"
        sourceEntry entry bufSink

    let correctDigest = "B9A995A6C7A9E75326CE524CA14D4DC7959F012A9E81BF0A5CD0E709767EDB63"
    unless (correctDigest == hex (hash res)) $ error "Wrong sha256 hash for pg244.txt"

