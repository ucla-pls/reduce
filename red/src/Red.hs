{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module Red where

-- filepath
import           System.FilePath

-- mtl
import           Control.Monad.Reader

-- directory
import           System.Directory

-- lens
import           Control.Lens

-- aeson
import           Data.Aeson

-- text
import qualified Data.Text.Lazy.Builder       as Builder

-- bytestring
import qualified Data.ByteString.Lazy.Char8   as BLC

-- optparse-applicative
import           Options.Applicative          as A

-- dirtree
import           System.DirTree

-- reduce-util
import           Control.Reduce.Graph
import           Control.Reduce.Reduction
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger   as L
import           Control.Reduce.Util.OptParse

-- base
import           Data.Char
import qualified Data.List                    as List
import           System.Exit


-- red
import           Control.Reduce.Language.C


data Format
  = FileFormat FileFormat
  | DirFormat DirFormat
  deriving (Show, Read, Ord, Eq)

data FileFormat
  = Chars
  | Lines
  | Json
  | CFile
  deriving (Show, Read, Ord, Eq)

data DirFormat
  = Files
  | FileTree
  deriving (Show, Read, Ord, Eq)

data MetricType
  = Counted
  | Displayed
  deriving (Show, Read, Ord, Eq)

parseTreeStrategy :: Parser TreeStrategy
parseTreeStrategy =
  toStrategy . map toLower
  <$> strOption
  ( short 'T' <> long "tree-strategy" <> value "graph"
    <> showDefault <> help "the reduction strategy for tree structures."
    <> metavar "STRATEGY"
  )
  where
    toStrategy str' =
      let f = List.isPrefixOf str' in
      if | f "graph" -> GraphStrategy
         | f "hdd" -> HddStrategy
         | f "flat" -> FlatStrategy
         | True ->
           error $ "Unknown format " ++ str'

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
      if | f "lines" -> FileFormat Lines
         | f "cfile" -> FileFormat CFile
         | f "chars" -> FileFormat Chars
         | f "json" -> FileFormat Json
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
  , _cnfDependencies     :: !(Maybe FilePath)
  , _cnfTreeStrategy     :: !TreeStrategy
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

  _cnfDependencies <- optional
    . strOption
    $ long "dependencies"
    <> short 'd'
    <> help "A csv file with edges between the dependencies. The headers should be 'from','to','label'."

  _cnfFormat <-
    parseFormat

  _cnfTreeStrategy <-
    parseTreeStrategy

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
    FileFormat CFile -> do
      (cedges, cfile) <- (liftIO $ parseCFilePre _cnfInputFile) >>= \case
        Right p ->
          return (cEdges p, RCTranslUnit p)
        Left msg -> logAndExit (display msg)

      liftIO $ print cedges

      let
        cmd = setup (inputFile (takeFileName _cnfInputFile) . printCFile ) $ makeCommand _cnfCommand
        problemDesc = setupProblem
          _cnfPredicateOptions
          (_cnfWorkFolder </> "baseline")
          cmd cfile

      problem <- withLogger problemDesc >>= \case
        Just problem -> return
          . meassure (counted "tokens")
          . toReductionTree cR
          $ problem
        Nothing ->
          logAndExit "Could not satisfy baseline"

      result <- handleErrors =<< withLogger
        ( runReduction
          _cnfReductionOptions
          (_cnfWorkFolder </> "iterations")
          (treeStrategy _cnfTreeStrategy _cnfReducerName)
          problem
        )

      output <- view cnfOutputFile
      L.phase ("Writing output to file " <> display output) $ do
        liftIO $ BLC.writeFile output (printCFile result)

    FileFormat ff -> do
      bs <- liftIO $ BLC.readFile _cnfInputFile

      let
        cmd = setup (inputFile "input") $ makeCommand _cnfCommand
        problemDesc = setupProblem _cnfPredicateOptions (_cnfWorkFolder </> "baseline") cmd bs

      abred <- withLogger problemDesc >>= \case
          Just problem -> do
            liftIO . print $ (expectation problem)
            case ff of
              Lines -> do
                let p = meassure (counted "lines") $ liftProblem BLC.lines BLC.unlines problem
                case _cnfDependencies of
                  Just csvfile
                    | takeExtension csvfile == ".csv" ->
                      readEdgesCSV <$> liftIO (BLC.readFile csvfile) >>= \case
                        Left msg ->
                          logAndExit $ "Error while reading CSV file:\n" <> displayString msg
                        Right (edges' :: [Edge () BLC.ByteString]) -> do
                          red <- intsetReduction <$> view cnfReducerName
                          return $ AbstractProblem red (toClosures edges' p)
                    | otherwise ->
                      logAndExit ("Unknown dependency format " <> displayString csvfile)
                  Nothing -> do
                    red <- listReduction <$> view cnfReducerName
                    return $ AbstractProblem red p
              Chars -> do
                red <- listReduction <$> view cnfReducerName
                return $ AbstractProblem red
                  ( meassure (counted "chars") . toStringified id
                    $ liftProblem BLC.unpack BLC.pack problem
                  )
              Json -> do
                return $ AbstractProblem (treeStrategy _cnfTreeStrategy _cnfReducerName)
                  ( meassure (counted "subtrees")
                    . toReductionTree jsonR
                    . refineProblem (
                      \s -> case decode s of
                        Just v  -> (encode, v)
                        Nothing -> fail "Could not decode the input"
                      )
                    $ problem
                  )
              CFile -> undefined
          Nothing ->
            logAndExit "Could not satisfy baseline"

      result <- handleErrors =<< withLogger
        ( runAbstractProblem _cnfReductionOptions (_cnfWorkFolder </> "iterations") abred )

      output <- view cnfOutputFile
      L.phase ("Writing output to file " <> display output) $ do
        liftIO $ BLC.writeFile output result

    DirFormat Files -> do
      dirtree <- liftIO $ readDirTree return _cnfInputFile

      dir <- case dirTreeNode dirtree of
        Directory dir -> return $ dir
        _             -> logAndExit "File not a folder"

      let cmd = setup (inputDirectoryWith (flip createFileLink) "input")
            $ makeCommand _cnfCommand

      problem <- withLogger
        ( setupProblem _cnfPredicateOptions (_cnfWorkFolder </> "baseline") cmd dir ) >>= \case
        Just problem ->
          return
          . meassure (counted "files")
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

    DirFormat FileTree -> do
      dirtree <- liftIO $ readDirTree makeAbsolute _cnfInputFile

      liftIO $ print dirtree

      let cmd = setup (inputDirTreeWith (flip createFileLink) "input")
            $ makeCommand _cnfCommand

      problem <- withLogger
        ( setupProblem _cnfPredicateOptions (_cnfWorkFolder </> "baseline")
          cmd
          dirtree
        ) >>= \case
        Just problem -> do
          return
            ( meassure (counted "files and folders")
              . toReductionTree dirtreeR
              $ problem
            )
        Nothing ->
          logAndExit "Could not satisfy baseline"

      result <- handleErrors =<< withLogger
        ( runReduction
          _cnfReductionOptions
          (_cnfWorkFolder </> "iterations")
          (treeStrategy _cnfTreeStrategy _cnfReducerName)
          problem
        )

      L.phase ("Writing output to file " <> display _cnfOutputFile) $ do
        liftIO . writeDirTree (flip copyFile) _cnfOutputFile $ result

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
