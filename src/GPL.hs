{-# LANGUAGE OverloadedStrings #-}
module GPL
    ( gpl
    , printGpl
    ) where

import Control.Monad
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as TI

gpl :: [Text]
gpl =
    [ "hascar, Copyright (C) 2016, Virtual Forge GmbH.", ""
    , "Maint.: Hans-Christian Esperer"
    , "<hans-christian.esperer@virtualforge.com>", ""
    , "hascar comes with ABSOLUTELY NO WARRANTY;"
    , "for details read the LICENSE file."
    , "This is free software, and you are welcome to redistribute"
    , "it under certain conditions; see the LICENSE file for details." ]

printGpl :: IO ()
printGpl = do
    TI.putStrLn "┌────────────────────────────────────────────────────────────────────┐"
    forM_ gpl $ \line -> do
        let len = T.length line
            padl = (68 - len) `quot` 2
            padr =  if padl + padl + len == 68
                    then padl
                    else padl + 1
            padl' = T.replicate padl " "
            padr' = T.replicate padr " "
        TI.putStrLn $ T.concat [ "│", padl', line, padr', "│" ]
    TI.putStrLn "└────────────────────────────────────────────────────────────────────┘"
