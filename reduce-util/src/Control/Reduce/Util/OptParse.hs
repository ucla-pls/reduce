{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
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

-- filepath
import           System.FilePath

-- directory
import           System.Directory

-- base
import           Data.Char                  (toLower)
import           Data.Foldable
import qualified Data.List                  as List

-- temporary
import System.IO.Temp

-- bytestring
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger
import           Control.Reduce.Util.CliPredicate

-- directory-tree
import System.Directory.Tree

data InputFrom
  = FromString String
  | FromFile FilePath
  deriving (Show, Eq)

-- data CmdOptionWithInput
--   = ArgumentOptions !String !(CmdOptions String)
--   | StreamOptions !BL.ByteString !(CmdOptions BL.ByteString)
--   | DirOptions !(DirTree FileContent) !(CmdOptions (DirTree FileContent))
--   deriving (Show)

-- data CmdOptionWithoutFormat =
--   CmdOptionWithoutFormat { withFormat :: forall a. InputFormat a -> IO (CmdOptions a)}

-- instance Show CmdOptionWithoutFormat where
--   showsPrec _ _ = showString "CmdOptionWithoutFormat"

parseCmd :: Parser (IO (Either String Cmd))
parseCmd =
  createCmd
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

-- parseCmdOptionsWithInput :: Parser (IO CmdOptionWithInput)
-- parseCmdOptionsWithInput =
--   getOptions
--   <$> parseInputFrom
--   <*> flag False True
--   (long "stream" <> short 'S' <> help "stream the input to the process.")
--   <*> parseCmdOptions
--   where
--     parseInputFrom = asum
--       [ FromString <$> strOption
--         (long "input" <> short 'i' <> help "the input to reduce.")
--       , FromFile <$> strOption
--         (long "file" <> short 'f' <> help "read input from file or folder.")
--       ]

--     getOptions ::
--       InputFrom
--       -> Bool
--       -> CmdOptionWithoutFormat
--       -> IO CmdOptionWithInput
--     getOptions input useStream fn =
--       case input of
--         FromFile fp -> do
--           isDir <- doesDirectoryExist fp
--           if isDir
--             then do
--               fp' <- canonicalizePath fp
--               rf <- fmap SameAs <$> readTree fp'
--               DirOptions (dirTree rf) <$> withFormat fn (DirInput $ takeFileName fp')
--             else do
--               rf <- BL.readFile fp
--               s <- if useStream
--                 then withFormat fn StreamInput
--                 else withFormat fn $ FileInput (takeFileName fp)
--               return $ StreamOptions rf s
--         FromString str' ->
--           if useStream
--             then do
--               s <- withFormat fn StreamInput
--               return $ StreamOptions (BLC.pack str') s
--             else do
--             s <- withFormat fn ArgsInput
--             return $ ArgumentOptions str' s

parsePredicateOptions :: String -> Parser (IO PredicateOptions)
parsePredicateOptions template =
  mkPredicateOptions
    <$> parseExitcode
    <*> switch (long "stdout" <> help "preserve stdout.")
    <*> switch (long "stderr" <> help "preserve stderr.")
    <*> ( Just <$> strOption
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
    <*> parseCmd
   where
     mkPredicateOptions ec so se wf kf mkCmd = do
       cmd <- mkCmd >>= \case
         Left err ->
           fail err
         Right cmd -> return $ cmd
       wf' <- case wf of
          Just folder -> do
            createDirectory folder
            return $ folder
          Nothing -> do
            createTempDirectory "." template
       return $ PredicateOptions ec so se wf' kf cmd

     parseExitcode =
       exitCodeFromInt
       <$> option auto
       ( long "exit-code"
         <> short 'E'
         <> help "preserve exit-code"
         <> value 0
         <> metavar "CODE"
         <> showDefault)

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

-- parseReducerOptions :: String -> Parser (IO ReducerOptions)
-- parseReducerOptions template =
--   mkReduceOptions
--   <$>

--   <*> (
--   Just <$> strOption
--     ( long "work-folder"
--       <> short 'W'
--       <> help "the work folder."
--       <> showDefault
--     )
--     <|> pure Nothing
--   )

--   <*> switch
--   ( long "keep-folders"
--     <> short 'K'
--     <> help "keep the work folders after use?"
--   )
--   where
--     mkReduceOptions red (mfolder :: Maybe FilePath) n = do
--       case mfolder of
--         Just folder -> do
--           createDirectory folder
--           return $ ReducerOptions red folder n
--         Nothing -> do
--           folder <- createTempDirectory "." template
--           return $ ReducerOptions red folder n

parseLogger :: Parser Logger
parseLogger =
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
