{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Red where

-- filepath
import           System.FilePath

-- mtl
import           Control.Monad.Reader

-- directory
import           System.Directory

-- lens
import           Control.Lens

-- text
import qualified Data.Text.Lazy.Builder       as Builder

-- bytestring
import qualified Data.ByteString.Lazy.Char8   as BLC

-- optparse-applicative
import           Options.Applicative          as A

-- dirtree
import System.DirTree

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Problem
import           Control.Reduce.Util.Logger   as L
import           Control.Reduce.Util.Metric
import           Control.Reduce.Util.OptParse

-- base
import           Data.Char
import qualified Data.List                    as List
import           System.Exit


data Format
  = FileFormat FileFormat
  | DirFormat DirFormat
  deriving (Show, Read, Ord, Eq)

data FileFormat
  = Chars
  | Lines
  deriving (Show, Read, Ord, Eq)

data DirFormat
  = Files
  | FileTree
  deriving (Show, Read, Ord, Eq)

data MetricType
  = Counted
  | Displayed
  deriving (Show, Read, Ord, Eq)

parseFormat :: Parser Format
parseFormat =
  toFormat . map toLower
  <$> strOption
  ( short 'F' <> long "format" <> value "lines"
    <> showDefault <> help "the format of the input."
    <> metavar "FORMAT"
  )
  where
    toFormat str' =
      let f = List.isPrefixOf str' in
      if | f "chars" -> FileFormat Chars
         | f "lines" -> FileFormat Lines
         | f "files" -> DirFormat Files
         | f "filetree" -> DirFormat FileTree
         | True ->
           error $ "Unknown format " ++ str'

parseMetricType :: Parser MetricType
parseMetricType =
  toMetricType . map toLower
  <$> strOption
  ( short 'm' <> value "counted"
    <> showDefault <> help "the metric type of the input."
    <> metavar "METRIC"
  )
  where
    toMetricType str' =
      let f = List.isPrefixOf str' in
      if | f "counted" -> Counted
         | f "displayed" -> Displayed
         | True ->
           error $ "Unknown metric type " ++ str'

data Config = Config
  { _cnfInputFile        :: !FilePath
  , _cnfOutputFile       :: !FilePath
  , _cnfLoggerConfig     :: !LoggerConfig
  , _cnfMetricType       :: !MetricType
  , _cnfFormat           :: !Format
  , _cnfReducerName      :: !ReducerName
  , _cnfWorkFolder       :: !FilePath
  , _cnfPredicateOptions :: !PredicateOptions
  , _cnfReductionOptions :: !ReductionOptions
  , _cnfCommand          :: !CommandTemplate
  } deriving (Show)

makeLenses ''Config

getConfigParser :: Parser (IO Config)
getConfigParser = do
  _cnfInputFile <- strOption $
    long "input-file"
    <> short 'i'

  _cnfOutputFile <- strOption $
    long "output-file"
    <> short 'o'

  _cnfLoggerConfig <-
    parseLoggerConfig

  _cnfMetricType <-
    parseMetricType

  _cnfFormat <-
    parseFormat

  _cnfReducerName <-
    parseReducerName

  ioWorkFolder <-
    parseWorkFolder "_red"

  _cnfPredicateOptions <-
    parsePredicateOptions

  _cnfReductionOptions <-
    parseReductionOptions

  ioCommand <-
    parseCommandTemplate

  pure $ do
    _cnfWorkFolder <- ioWorkFolder
    _cnfCommand <- either fail return =<< ioCommand
    return $ Config {..}

instance HasLogger Config where
  loggerL = cnfLoggerConfig

main :: IO ()
main = do
  config <- join . execParser $
    A.info (getConfigParser <**> helper)
    ( fullDesc
    <> header "red"
    <> progDesc "A command line tool for reducing almost anything."
    )
  runReaderT run config

run :: ReaderT Config IO ()
run = do
  Config {..} <- ask

  case _cnfFormat of
    FileFormat ff -> do
      bs <- liftIO $ BLC.readFile _cnfInputFile

      let cmd = setup (inputFile "input") $ makeCommand _cnfCommand

      problem <- withLogger
        ( setupProblem _cnfPredicateOptions (_cnfWorkFolder </> "baseline") cmd bs ) >>= \case
          Just problem -> return $ case ff of
            Lines ->
              meassure counted . toIndexed
              $ liftProblem BLC.lines BLC.unlines problem
            Chars ->
              meassure counted . toStringified id
              $ liftProblem BLC.unpack BLC.pack problem
          Nothing ->
            logAndExit "Could not satisfy baseline"

      red <- listReduction <$> view cnfReducerName

      result <- handleErrors =<< (withLogger $
        runReduction _cnfReductionOptions (_cnfWorkFolder </> "iterations") red problem)

      output <- view cnfOutputFile
      L.phase ("Writing output to file " <> display output) $ do
        liftIO $ BLC.writeFile output result

    DirFormat _ -> do

      dirtree <- liftIO $ readDirTree return _cnfInputFile

      dir <- case dirTreeNode dirtree of
        Directory dir -> return $ dir
        _ -> logAndExit "File not a folder"

      let cmd = setup (inputDirectoryWith (flip createFileLink) "input") $ makeCommand _cnfCommand

      problem <- withLogger
        ( setupProblem _cnfPredicateOptions (_cnfWorkFolder </> "baseline") cmd dir ) >>= \case
        Just problem ->
          return
          . meassure counted
          . toStringified (\(key, _) -> last . show . length $ key)
          . liftProblem toDeepFileList fromDeepFileList
          $ problem
        Nothing ->
          logAndExit "Could not satisfy baseline"

      red <- listReduction <$> view cnfReducerName

      result <- handleErrors =<< (withLogger $
        runReduction _cnfReductionOptions (_cnfWorkFolder </> "iterations") red problem)

      output <- view cnfOutputFile
      L.phase ("Writing output to directory " <> display output) $ do
        liftIO . writeDirTree (flip copyFile) output . directory $ result

  where
    handleErrors (s, r) = do
      case s of
        Just ReductionTimedOut ->
          L.warn "Reduction timed out"
        Just ReductionIterationsExceeded ->
          L.warn "The max iterations reached while reducing"
        Just ReductionFailed ->
          L.warn "No reduction possible"
        Nothing ->
          return ()
      return r

logAndExit ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m b
logAndExit bldr = do
  L.err bldr
  liftIO $ exitWith (ExitFailure 1)
