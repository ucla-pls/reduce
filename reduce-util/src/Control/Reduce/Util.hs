{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Control.Reduce.Util
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module provides utils, so that it is easier to write command-line reducers.

* TODO

- Handle Ctrl^C
-}
module Control.Reduce.Util
  ( -- reduce

    mkCliPredicate

  , CliOptions (..)
  , mkCliOptions
  , setTest

  , fromCommand

  , Test (..)

  , consume
  -- | Re-export Control.Reduce for convenience:
  , module Control.Reduce
  ) where

-- async
import Control.Concurrent.Async

-- typed-process
import System.Process.Typed

-- filepath
import System.FilePath

-- directory
import System.Directory

-- bytestring
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

-- cryptohash-sha256
import Crypto.Hash.SHA256 as SHA256

-- stm
import Control.Concurrent.STM

-- base
import System.Exit
import System.IO.Error
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Word
import Data.Functor
import Data.Foldable

-- contravariant
import Data.Functor.Contravariant

-- reduce-util
import System.Process.Consume

-- reduce
import Control.Reduce
import Data.Functor.Contravariant.PredicateM

-- | The options to build a command-line reducer.
data ReducerOptions a = ReducerOptions
  {
    -- | The underlying reducer to be used to reduce the item.
    reducer :: Reducer IO a

    -- | Command Line Options
  , cliOptions :: CliOptions
  }

-- -- | Runs the reducer is the IO monad
-- reduce :: ReducerOptions FilePath -> a -> IO (Maybe a)
-- reduce ReducerOptions {..} a = do
--   absoluteCommand <- canonicalizePath command
--   ref <- newIORef (0 :: Int)
--   reducer ( predicate absoluteCommand ref ) a
--   where
--     predicate absoluteCommand ref simpl = do
--       i <- atomicModifyIORef' ref (\i -> ( succ i, i))

--       let folder = workFolder </> printf "%04o" i

--       createDirectoryIfMissing True folder

--       success <- withCurrentDirectory folder $ do
--         file <- toFile simpl
--         (ec, _, _ ) <- readProcess ( proc absoluteCommand ( args ++ [file] ) )
--         return (ec == ExitSuccess)

--       when
--         ( not $ keepIterations )
--         ( removePathForcibly folder )

--       return success
data CliOptions = CliOptions
  { test       :: ! [Test]
  , cmd        :: ! FilePath
  , args       :: ! [String]
  , workFolder :: ! (Maybe FilePath)
  }

getExecutable :: String -> IO FilePath
getExecutable exec = do
  findExecutable exec >>=
    maybe (canonicalizePath exec) return

mkCliOptions :: FilePath -> IO CliOptions
mkCliOptions fp = do
  cfp <- getExecutable fp
  return $ CliOptions
    { test = []
    , cmd = cfp
    , args = []
    , workFolder = Nothing
    }

setTest :: [Test] -> CliOptions -> CliOptions
setTest ts cl =
  cl { test = ts }

setWorkFolder :: Maybe FilePath -> CliOptions -> CliOptions
setWorkFolder ts cl =
  cl { workFolder = ts }

data Test
  = Status ExitCode
  | StdOutHash SHA256
  | StdErrHash SHA256

-- | We can create a 'FilePathPredicate' from a command.
mkCliPredicate :: CliOptions -> PredicateM IO FilePath
mkCliPredicate CliOptions {..} =
  contramap fileNameToProc $ testCommand test
  where
    fileNameToProc filepath =
      maybe id setWorkingDir workFolder
      $ proc cmd (args ++ [filepath])

-- | We can create a 'FilePathPredicate' from a command.
fromCommand :: [Test] -> FilePath -> [String] -> PredicateM IO FilePath
fromCommand test cmd args = do
  mkCmdPredicate (\filepath -> return $ proc cmd (args ++ [filepath])) test

mkCmdPredicate :: (a -> IO (ProcessConfig () () ())) -> [Test] -> PredicateM IO a
mkCmdPredicate fn test  = do
  contramapM fn (testCommand test)

testCommand :: [Test] -> PredicateM IO (ProcessConfig () () ())
testCommand test =
  consumeWithHash ignoreConsumer ignoreConsumer
  `contramapM` contramap testp ifTrueT
  where
    testp (ec, ((), out), ((), err)) =
      flip all test $ \case
        Status ec' -> ec == ec'
        StdOutHash hash -> hash == out
        StdErrHash hash -> hash == err

-- -- | An optparser
-- reducerOptionsOpt ::
--   FilePath
--   -> Reducer [a] IO
--   -> Parser (([a] -> IO FilePath) -> ReducerOptions [a])
-- reducerOptionsOpt workFolder reducer =
--   ReducerOptions
--   <$> ( strOption
--         ( long "work-folder" <> short 'w' <> value workFolder
--           <> showDefault <> help "The folder to work in folder"
--         ))
--   <*> ( ( flag' ddmin (long "ddmin" <> help "Use ddmin")
--         <|> flag' binaryReduction (long "bired" <> help "Use binary reduction")
--         <|> flag' linaryReduction (long "lired" <> help "Use linary reduction")
--         ) <|> pure reducer)
--   <*> ( switch (long "keep-iterations" <> short 'k') )

--   <*> many ( strArgument ( metavar "ARGS.." <> help "Optional arguments to the predicate script" ))
