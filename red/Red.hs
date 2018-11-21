{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- The red application

-- mtl
import Control.Monad.Reader

-- text
import qualified Data.Text as Text

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- optparse-applicative
import Options.Applicative

-- reduce-util
import Control.Reduce.Util
import System.Process.Consume

-- reduce
import Control.Reduce

-- base
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import Data.Foldable

data Format = Format
  { formatFlag :: !Char
  , formatDescription :: !String
  } deriving (Show, Eq)

data Config = Config
  { format :: !Format
  , checkOptions :: !CheckOptions
  , cmdOptionsWithInput :: !CmdOptionWithInput
  } deriving (Show)

getConfigParser :: [ Format ] -> Parser (IO Config)
getConfigParser formats =
  cfg
  <$> asum (map formatAsOpt formats)
  <*> parseCheckOptions
  <*> parseCmdOptionsWithInput
  where
    formatAsOpt fmt@(Format f desc) =
      flag' fmt (short f <> help desc)

    cfg fmt check cmd =
      Config fmt check <$> cmd

main = do
  let configParser = getConfigParser formats
  config <- join . execParser $
    info (configParser <**> helper)
    ( fullDesc
    <> header "red"
    <> progDesc "A command line tool for reducing almost anything."
    )

  loggers <- mkLoggers
  runReaderT (run config) loggers
  where
    formats =
      [ Format 'c' "see the input as a list of chars"
      , Format 'l' "see the input as a list of lines"
      ]

run :: Config -> ReaderT Loggers IO ()
run Config {..} = do
  case cmdOptionsWithInput of
    StreamOptions a cmdOptions -> do
      mp <- toPredicateM checkOptions cmdOptions a
      case mp of
        Just p -> liftIO . print =<< runPredicateM p a
        Nothing -> liftIO $print "failed"
    ArgumentOptions a cmdOptions -> do
      mp <- toPredicateM checkOptions cmdOptions a
      case mp of
        Just p -> liftIO . print =<< runPredicateM p a
        Nothing -> liftIO $print "failed"
