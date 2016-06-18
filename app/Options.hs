module Options
    ( Options(..)
    , run
    , runWithHeader
    ) where

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
    } deriving (Show)

runWithHeader :: (Options -> IO a) -> IO a
runWithHeader o = run (\l -> putGpl >> o l)

run :: (Options -> IO a) -> IO a
run = (>>=) (execParser spec)

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


