{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Red where

-- mtl
import           Control.Monad.Reader

-- unliftio
import           UnliftIO

-- lens
import           Control.Lens
import           Control.Lens.Iso

-- text
import qualified Data.Text.Lazy.Builder                as Builder

-- bytestring
import qualified Data.ByteString.Lazy.Char8            as BLC

-- optparse-applicative
import           Options.Applicative                   as A

-- vector
import qualified Data.Vector                           as V

-- reduce
import           Data.Functor.Contravariant.PredicateM

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.CliPredicate
import           Control.Reduce.Util.Logger            as L
import           Control.Reduce.Util.OptParse
import           System.Directory.Tree

-- cassava
import qualified Data.Csv                              as C

-- contravariant
import           Data.Functor.Contravariant

-- base
import           Data.Char
import           Data.Foldable
import qualified Data.List                             as List
import           Data.Maybe
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
  ( short 'f' <> value "lines"
    <> showDefault <> help "the format of the input."
    <> metavar "FORMAT"
  )
  where
    toFormat str =
      let f = List.isPrefixOf str in
      if | f "chars" -> FileFormat Chars
         | f "lines" -> FileFormat Lines
         | f "files" -> DirFormat Files
         | f "filetree" -> DirFormat FileTree
         | True ->
           error $ "Unknown format " ++ str

parseMetricType :: Parser MetricType
parseMetricType =
  toMetricType . map toLower
  <$> strOption
  ( short 'm' <> value "counted"
    <> showDefault <> help "the metric type of the input."
    <> metavar "METRIC"
  )
  where
    toMetricType str =
      let f = List.isPrefixOf str in
      if | f "counted" -> Counted
         | f "displayed" -> Displayed
         | True ->
           error $ "Unknown metric type " ++ str

data Config = Config
  { _cnfInputFile       :: !FilePath
  , _cnfOutputFile      :: !FilePath
  , _cnfLoggerConfig    :: !LoggerConfig
  , _cnfMetricType      :: !MetricType
  , _cnfFormat          :: !Format
  , _cnfReducionOptions :: !ReductionOptions
  , _cnfCommand         :: !CommandTemplate
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

  ioReductionOptions <-
    parseReductionOptions "red"

  ioCommand <-
    parseCommandTemplate

  pure $ do
    _cnfReducionOptions <- ioReductionOptions
    _cnfCommand <- either fail return =<< ioCommand
    return $ Config {..}
  where
    cfg input output lg mtype rn fmt cmd =
      Config input output lg mtype rn fmt <$> cmd

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
  template <- view cnfCommand

  view cnfFormat >>= \case
    FileFormat ff -> do
      input <- view cnfInputFile
      bs <- liftIO $ BLC.readFile input

      let command = setup (inputFile "input") $ makeCommand template

      base <- withLogger $ baseline "here" command bs

      result <- case ff of
        Lines -> do
          return bs
          -- result <- reducex (fromFile "input") . counted BLC.unlines $ BLC.lines bs
        -- Chars -> do
        --   case cnfMetricType of
        --     Counted -> reducex
        --       (fromFile "input") (counted BLC.unpack BLC.pack ) bs
        --     Displayed -> reducex
        --       (fromFile "input") (displayed bs id BLC.unpack BLC.pack) bs

      output <- view cnfOutputFile
      liftIO $ BLC.writeFile output result
    -- DirFormat df -> do
    --   case df of
    --     Files -> do
    --       _ :/ dt <- liftIO $ fmap SameAs <$> readTree cnfInputFile
    --       result <- reducex
    --         (fromDirTree "input")
    --         (counted toFileList (fromJust . fromFileList))
    --         dt
    --       liftIO $ writeTreeWith writeContent (cnfOutputFile :/ result)
  -- where
  --   reducex ::
  --     (HasLogger env, MonadReader env m, MonadUnliftIO m, Metric x)
  --     => (b -> m CmdInput)
  --     -> ReductionModel x b [a]
  --     -> m b
  --   reducex cmdIn model =
  --     mkReductionProblem cnfPredicateOptions cmdIn  \case
  --       Just p -> do
  --         reduce cnfReducerName (applyModel model p) >>= \case
  --           Just result -> do
  --             return result
  --           Nothing -> do
  --             L.warn "Could not reduce problem"
  --             return b
  --       Nothing -> do
  --         L.err "Predicate failed"
  --         liftIO $ exitWith (ExitFailure 1)


    -- reduceAll ::
    --   (HasLogger env, MonadReader env m, MonadUnliftIO m, Metric a)
    --   => (a -> m CmdInput)
    --   -> a
    --   -> m a
    -- reduceAll tofile a =
    --   toPredicateM cnfPredicateOptions tofile (f bs) >>= \case
    --     Just predicate -> do
    --       result <- reduce cnfReducerName Nothing predicate f bs
    --       case result of
    --         Just r -> return r
    --         Nothing -> do
    --           L.warn "Could not reduce problem"
    --           return (f $ bs)
    --     Nothing -> do
    --       L.err "Predicate failed"
    --       liftIO $ exitWith (ExitFailure 1)


logAndExit ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m b
logAndExit bldr = do
  L.err bldr
  liftIO $ exitWith (ExitFailure 1)
