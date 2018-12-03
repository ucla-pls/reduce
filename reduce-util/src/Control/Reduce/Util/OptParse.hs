{-# LANGUAGE LambdaCase          #-}
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
import System.Directory

-- base
import           Data.Foldable
import           Data.Char (toLower)
import qualified Data.List                  as List

-- bytestring
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BLC

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger

data InputFrom
  = FromString String
  | FromFile FilePath
  deriving (Show, Eq)

parseCmdOptionsWithInput :: Parser (IO CmdOptionWithInput)
parseCmdOptionsWithInput =
  getOptions
  <$> ( asum
       [ FromString <$> strOption
         (long "input" <> short 'i' <> help "the input to code.")
       , FromFile <$> strOption
         (long "file" <> short 'f' <> help "read input from file or folder.")
       ])
  <*> flag False True
  (long "stream" <> short 'S' <> help "stream the input to the process.")
  <*> option auto
  (long "timelimit"
   <> short 'T'
   <> metavar "SECS"
   <> value (-1)
   <> showDefault
   <> help (
      "the maximum number of seconds to run the process,"
      ++ " negative means no timelimit."))
  <*> strArgument
  (metavar "CMD" <> help "the command to run")
  <*> many
  (strArgument (metavar "ARG.." <> help "arguments to the command."))
  where
    getOptions input useStream tl c args' = do
      if useStream
        then do
        s <- mkCmdOptions (StreamInput) tl c args'
        case input of
          FromFile fp -> do
            rf <- BLC.fromStrict <$> BS.readFile fp
            return $ StreamOptions rf s
          FromString str' ->
            return $ StreamOptions (BLC.pack str') s
        else do
        s <- mkCmdOptions (ArgsInput) tl c args'
        case input of
          FromFile fp -> do
            return $ ArgumentOptions fp s
          FromString str' ->
            return $ ArgumentOptions str' s

parseCheckOptions :: Parser CheckOptions
parseCheckOptions =
  CheckOptions
      <$> ( exitCodeFromInt
            <$> option auto
            ( long "exit-code"
              <> short 'E'
              <> help "preserve exit-code"
              <> value 0
              <> metavar "CODE"
              <> showDefault)
          )
      <*> switch (long "stdout" <> help "preserve stdout.")
      <*> switch (long "stderr" <> help "preserve stderr.")

reducerNameFromString :: String -> Maybe ReducerName
reducerNameFromString = \case
  "ddmin" -> Just Ddmin
  "linear" -> Just Linear
  "binary" -> Just Binary
  _ -> Nothing

parseReducerOptions :: Parser (IO ReducerOptions)
parseReducerOptions =
  mkReduceOptions
  <$> (
  option (maybeReader (reducerNameFromString . map toLower))
    ( long "reducer"
      <> short 'R'
      <> help "the reducing algorithm to use."
      <> value Binary
      <> showDefault
    )
  )

  <*> (
  Just <$> strOption
    ( long "work-folder"
      <> short 'W'
      <> help "the work folder."
      <> showDefault
    )
    <|> pure Nothing
  )

  <*> switch
  ( long "keep-folders"
    <> short 'K'
    <> help "keep the work folders after use?"
  )
  where
    mkReduceOptions red (mfolder :: Maybe FilePath) n = do
      case mfolder of
        Just folder -> do
          createDirectory folder
          return $ ReducerOptions red folder n
        Nothing ->
          error "please set the work folder for now."

parseSimpleLogger :: Parser (SimpleLogger)
parseSimpleLogger =
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
      boundedToEnum (1 - lvl + quiet)


boundedToEnum :: (Bounded a, Enum a) => Int -> a
boundedToEnum i =
  maybe maxBound id
  . fmap fst
  . List.uncons
  . drop i
  $ enumFrom minBound
