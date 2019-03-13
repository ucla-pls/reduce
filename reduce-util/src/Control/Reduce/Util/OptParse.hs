{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Control.Reduce.Util.OptParse
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module contains the options parsers for the config files of
Control.Reduce.Util.
-}

module Control.Reduce.Util.OptParse where

-- optparse-applicative
import           Options.Applicative

-- directory
import           System.Directory

-- base
import           Data.Char                        (toLower)
import qualified Data.List                        as List
import           Control.Monad

-- temporary
import           System.IO.Temp

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger

data InputFrom
  = FromString String
  | FromFile FilePath
  deriving (Show, Eq)

parseCommandTemplate :: Parser (IO (Either String CommandTemplate))
parseCommandTemplate =
  createCommandTemplate
  <$> option auto
  ( long "timelimit"
    <> short 'T'
    <> metavar "SECS"
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
  predOptPreserveExitCode <-
    not <$> switch (long "no-exitcode" <> help "ignore the exitcode.")

  predOptPreserveStderr <-
    switch (long "stdout" <> help "preserve stdout.")

  predOptPreserveStdout <-
    switch (long "stderr" <> help "preserve stderr.")

  pure $ PredicateOptions {..}

parseWorkFolder :: String -> Parser (IO FilePath)
parseWorkFolder template = do
  workFolder <-
    Just <$> strOption
    ( long "work-folder"
      <> short 'W'
      <> help "the work folder."
      <> showDefault
    )
    <|> pure Nothing

  useForce <-
    switch ( long "force" <> short 'f' <> help "delete the work folder if it already exists." )

  pure $
    ioWorkFolder workFolder useForce
  where
    ioWorkFolder workFolder useForce =
      makeAbsolute =<< case workFolder of
        Just folder -> do
          when useForce $ removePathForcibly folder
          createDirectory folder
          return $ folder
        Nothing -> do
          createTempDirectory "." template


parseReductionOptions :: Parser (ReductionOptions)
parseReductionOptions = do
  redOptTotalTimeout <- option auto $
    long "total-time"
      <> metavar "SECS"
      <> value (-1)
      <> showDefault
      <> help "the maximum seconds to run all predicates, negative means no timelimit."

  redOptMaxIterations <- option auto $
    long "max-iterations"
    <> metavar "ITERS"
    <> value (-1)
    <> showDefault
    <> help "the maximum number of time to run the predicate, negative means no limit."

  redOptKeepFolders <- switch $
    long "keep-folders"
    <> short 'K'
    <> help "keep the work folders after use?"

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
reducerNameFromString = \case
  "ddmin" -> Just Ddmin
  "linear" -> Just Linear
  "binary" -> Just Binary
  _ -> Nothing

parseReducerName :: Parser ReducerName
parseReducerName =
  option (maybeReader (reducerNameFromString . map toLower))
  ( long "reducer"
    <> short 'R'
    <> help "the reducing algorithm to use."
    <> value Binary
    <> showDefault
  )

parseLoggerConfig :: Parser LoggerConfig
parseLoggerConfig =
  mklogger
  <$> ( parseLogLevel
        <$> (length <$> many (flag' () (short 'v' <> help "make it more verbose.")))
        <*> (length <$> many (flag' () (short 'q' <> help "make it more quiet.")))
      )
  <*> option auto
  ( short 'D'
    <> long "log-depth"
    <> help "set the log depth."
    <> value (-1)
    <> showDefault
  )
  where
    mklogger ll depth =
      defaultLogger { logLevel = ll, maxDepth = depth }

    parseLogLevel lvl quiet =
      boundedToEnum (2 - lvl + quiet)


boundedToEnum :: (Bounded a, Enum a) => Int -> a
boundedToEnum i =
  maybe maxBound id
  . fmap fst
  . List.uncons
  . drop i
  $ enumFrom minBound
