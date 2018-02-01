{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GPL
    ( gpl
    , gpl'
    , putGpl
    ) where

import Control.Monad
import Data.Text (Text)
import Development.GitRev
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Text as T
import qualified Data.Text.IO as TI

-- | The copyright text
gpl' :: [Text]
gpl' =
    [ "hascar, Copyright (C) 2015-2018, Virtual Forge GmbH.", ""
    , "hascar comes with ABSOLUTELY NO WARRANTY;"
    , "for details read the LICENSE file."
    , "This is free software, and you are welcome to redistribute"
    , "it under certain conditions; see the LICENSE file for details." ]

-- | Print the header to the screen
putGpl :: IO ()
putGpl = putDoc gpl >> putStrLn ""

-- | Nicely formatted copyright text
gpl  :: Doc
gpl  = hsep . map (\l -> text (T.unpack l) <> line) $ lines
    where
        header  = " ┌────────────────────────────────────────────────────────────────────┐"
        lines   :: [Text]
        lines   =  [ header ] ++ map pad gpl' ++ map pad
            [ "", T.pack version ] ++ [ footer ]
        footer  = "└────────────────────────────────────────────────────────────────────┘"

-- | Center a text with box characters at the leftmost
-- and rightmost position
pad :: Text -> Text
pad line = T.concat [ "│", padl', line, padr', "│" ]
    where
        len = T.length line
        padl = (68 - len) `quot` 2
        padr =  if padl + padl + len == 68
                then padl
                else padl + 1
        padl' = T.replicate padl " "
        padr' = T.replicate padr " "

version :: String
version = "hascar " ++ take 8 $(gitHash) ++ " revision " ++ $(gitCommitCount) ++ " on branch " ++ $(gitBranch)
