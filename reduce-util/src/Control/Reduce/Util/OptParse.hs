{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Control.Reduce.Util.OptParse
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module contains the options parsers for the config files of
Control.Reduce.Util.
-}

module Control.Reduce.Util.OptParse where

-- optparse-applicative
import           Options.Applicative

-- directory
import           System.Directory

-- text
import qualified Data.Text as Text

-- base
import           Data.Char                        (toLower)
import qualified Data.List                        as List
import           Control.Monad
import           Control.Monad.IO.Class

-- unlifio
import           UnliftIO.Temporary
import           UnliftIO

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Command
import           Control.Reduce.Util.Logger

data InputFrom
  = FromString String
  | FromFile FilePath
  deriving (Show, Eq)

parseCmdTemplate :: Parser (IO (Either String CmdTemplate))
parseCmdTemplate =
  createCmdTemplate
  <$> option auto
  ( long "timelimit"
    <> short 'T'
    <> metavar "SECS"
    <> hidden
    <> value (-1)
    <> showDefault
    <> help (
        "the maximum number of seconds to run the process,"
        ++ " negative means no timelimit.")
  )
  <*> strArgument
  ( metavar "CMD" <> help "the command to run"
  )
  <*> many
  ( strArgument (metavar "ARG.." <> help "arguments to the command.")
  )

parsePredicateOptions :: Parser PredicateOptions
parsePredicateOptions = do
  input <-
    option str
    ( long "preserve"
      <> short 'p'
      <> help "a comma separated list of what to preserve. Choose from out, err, and exit. "
      <> value "exit"
      <> showDefault
      <> hidden
      )

  _predOptPriorExpectation <-
    parseExpectation

  pure $
    let ms = map Text.strip . Text.splitOn "," . Text.pack $ input
    in PredicateOptions
    { _predOptPreserveExitCode = List.elem "exit" ms || List.elem "exitcode" ms
    , _predOptPreserveStdout = List.elem "out" ms || List.elem "stdout" ms
    , _predOptPreserveStderr = List.elem "err" ms || List.elem "stderr" ms
    , _predOptPriorExpectation = _predOptPriorExpectation
    }

data WorkFolder
  = TempWorkFolder !String
  | DefinedWorkFolder !Bool !FilePath
  deriving (Show, Eq)

withWorkFolder :: (MonadUnliftIO m) => WorkFolder -> (FilePath -> m a) -> m a
withWorkFolder wf fn =
  case wf of
    DefinedWorkFolder useForce folder -> do
      fld <- liftIO $ do
        when useForce $ removePathForcibly folder
        createDirectory folder
        makeAbsolute folder
      fn fld
    TempWorkFolder template -> do
      withSystemTempDirectory template fn

parseWorkFolder :: String -> Parser WorkFolder
parseWorkFolder template = do
  workFolder <-
    Just <$> strOption
    ( long "work-folder"
      <> short 'W'
      <> help "the work folder."
      <> hidden
      <> showDefault
    )
    <|> pure Nothing

  useForce <- switch $
    long "force"
    <> short 'f'
    <> hidden
    <> help "delete the work folder if it already exists."

  pure $ case workFolder of
    Just folder -> do
      DefinedWorkFolder useForce folder
    Nothing -> do
      TempWorkFolder $ template

parseExpectation :: Parser Expectation
parseExpectation = do
  _expectedExitCode <-
    optional . fmap exitCodeFromInt . option auto $
    long "exit"
    <> help "require a specific exit code."
    <> hidden

  _expectedStdout <- pure Nothing
    -- optional . option auto $
    -- long "out"
    -- <> help "require a specific stdout hash."
    -- <> hidden

  _expectedStderr <- pure Nothing
    -- optional . option auto $
    -- long "err"
    -- <> help "require a specific stderr hash."
    -- <> hidden

  pure $ Expectation {..}

parseReductionOptions :: Parser (ReductionOptions)
parseReductionOptions = do
  _redTotalTimelimit <- option auto $
    long "total-time"
      <> metavar "SECS"
      <> value (-1)
      <> showDefault
      <> hidden
      <> help "the maximum seconds to run all predicates, negative means no timelimit."

  _redMaxIterations <- option auto $
    long "max-iterations"
    <> metavar "ITERS"
    <> value (-1)
    <> showDefault
    <> hidden
    <> help "the maximum number of time to run the predicate, negative means no limit."

  _redKeepFolders <- switch $
    long "keep-folders"
    <> hidden
    <> help "keep the reduction folders after use?"
  
  _redKeepOutputs <- switch $
    long "keep-outputs"
    <> hidden
    <> help "keep the stdout and stderr outputs?"

  _redMetricsFile <- strOption $
    long "metrics-file"
    <> hidden
    <> showDefault
    <> value "metrics.csv"
    <> help "an absolute or relative (to the WORKFOLDER/reduction) path of the metric file"

  _redPredicateTimelimit <- option auto $
    long "predicate-timelimit"
    <> hidden
    <> showDefault
    <> value (-1)
    <> help "the timelimit of the predicate in seconds, negative means no limit"

  _redTryInitial <- switch $
    long "try-initial"
    <> hidden
    <> showDefault
    <> help "try the intitial problem, recored as (0000)"
  
  _redFailFast <- switch $
    long "fail-fast"
    <> hidden
    <> showDefault
    <> help "exit quickly on errors, especially if the intitial try was bad."

  pure $ ReductionOptions {..}

  -- where
  --   parseExitcode =
  --     exitCodeFromInt
  --     <$> option auto
  --     ( long "exit-code"
  --       <> short 'E'
  --       <> help "preserve exit-code"
  --       <> value 0
  --       <> metavar "CODE"
  --       <> showDefault)

reducerNameFromString :: String -> Maybe ReducerName
reducerNameFromString a
  | match "ddmin" = Just Ddmin
  | match "linear" = Just Linear
  | match "binary" = Just Binary
  | otherwise = Nothing
  where
    match = Text.isPrefixOf (Text.toLower . Text.pack $ a)

parseReducerName :: Parser ReducerName
parseReducerName =
  option (maybeReader (reducerNameFromString . map toLower))
  ( long "reducer"
    <> short 'R'
    <> help "the reducing algorithm to use. Choose from ddmin, linear, or binary. (default: \"binary\")"
    <> metavar "RED"
    <> value Binary
  )

parseLoggerConfig :: Parser LoggerConfig
parseLoggerConfig =
  mklogger
  <$> ( parseLogLevel
        <$> (length <$> many (flag' () (short 'v' <> hidden <> help "make it more verbose.")))
        <*> (length <$> many (flag' () (short 'q' <> hidden <> help "make it more quiet.")))
      )
  <*> option auto
  ( short 'D'
    <> long "log-depth"
    <> help "set the log depth."
    <> hidden
    <> value (-1)
    <> showDefault
  )
  where
    mklogger ll depth =
      defaultLogger { logLevel = ll, maxDepth = depth }

    parseLogLevel lvl quiet =
      boundedToEnum (2 - lvl + quiet)

parseOutputFile :: Parser (Maybe FilePath)
parseOutputFile =
  optional . strOption $
    long "output-file"
    <> hidden
    <> metavar "OUTPUT"
    <> help "specifies where to put the output"
    <> short 'o'


boundedToEnum :: (Bounded a, Enum a) => Int -> a
boundedToEnum i =
  maybe maxBound id
  . fmap fst
  . List.uncons
  . drop i
  $ enumFrom minBound
