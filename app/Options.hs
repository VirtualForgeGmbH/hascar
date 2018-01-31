module Options
    ( Options(..)
    , run
    , run'
    ) where

import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative

import qualified Data.Text as T

import GPL

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
    , -- | Try to extract transports from PAT files
      oExtractPatFiles      :: !Bool
    } deriving (Show)

run :: (Options -> IO a) -> IO a
run o = run' (\l -> unless (oQuiet l) putGpl >> o l)

run' :: (Options -> IO a) -> IO a
run' = (>>=) (execParser spec)

spec :: ParserInfo Options
spec = info (helper <*> optionsParser)
     (  fullDesc
     <> progDesc "Decompress SAPCAR archives"
     <> headerDoc (Just gpl) )

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
    <*> switch
        (  long "depat"
        <> short 'p'
        <> help "Try to extract transport files from PAT files" )


