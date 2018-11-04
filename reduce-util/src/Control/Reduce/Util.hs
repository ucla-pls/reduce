{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
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

  fromCommand

  , Test (..)

  , consume
  , ConsumerOptions (..)
  , defaultConsumerOptions
  , lineLogger
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
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

-- cryptohash-sha256
import Crypto.Hash.SHA256 as SHA256

-- stm
import Control.Concurrent.STM

-- base
import System.Exit
import System.IO.Error
import Control.Monad
import Data.IORef
import Data.Word
import Data.Functor
import Data.Foldable

-- reduce
import Control.Reduce

-- -- | The options to build a command-line reducer.
-- data ReducerOptions a = ReducerOptions
--   {
--     -- | The underlying reducer to be used to reduce the item.
--     reducer :: Reducer IO a

--   -- , keepIterations :: !Bool
--   -- -- ^ Keep the intermediate folders around
--   -- , command :: !FilePath
--   -- -- ^ The command
--   -- , args :: ![String]
--   -- -- ^ Arguments to the path

--   --   workFolder :: !FilePath
--   -- -- ^ The folder to run in
--   -- , toFile :: a -> IO FilePath
--   -- -- ^ Convert the `a` to an argument
--   }


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


-- | A filepath predicate is a predicate, that is based on a filepath.
type FilePathPredicate = Predicate IO FilePath

data Test
  = Status ExitCode
  | StdOutHash SHA256
  | StdErrHash SHA256

-- | We can create a 'FilePathPredicate' from a command.
fromCommand :: ConsumerOptions -> [Test] -> FilePath -> [String] -> FilePathPredicate
fromCommand options test cmd args filepath = do
  (ec, out, err) <- consume options cmd (args ++ [filepath])
  return . flip all test $ \case
    Status ec' -> ec == ec'
    StdOutHash hash -> hash == out
    StdErrHash hash -> hash == err

type LogFunc = BS.ByteString -> IO ()

data ConsumerOptions = ConsumerOptions
  { outLogger :: LogFunc
  , errLogger :: LogFunc
  , workingFolder :: (Maybe FilePath)
  }

defaultConsumerOptions =
  ConsumerOptions (const $ return ()) (const $ return ()) Nothing

type SHA256 = (BS.ByteString, Word64)

-- | Consumes the output of a command and condenses it into a sha256
consume ::
  ConsumerOptions
  -> FilePath
  -> [String]
  -> IO (ExitCode, SHA256, SHA256)
consume ConsumerOptions {..} cmd args = do
  withProcess cfg $ \p -> do
    out <- async (SHA256.finalizeAndLength <$> logger outLogger (getStdout p) SHA256.init)
    err <- async (SHA256.finalizeAndLength <$> logger errLogger (getStderr p) SHA256.init)
    atomically $
      (,,) <$> waitExitCodeSTM p <*> waitSTM out <*> waitSTM err
  where
    cfg =
      setStderr createPipe
      . setStdout createPipe
      . maybe id setWorkingDir workingFolder
      $ proc cmd args

    logger fn handle ctx = do
      str <- BS.hGetSome handle 256
      fn str
      if BS.null str
        then return ctx
        else logger fn handle (SHA256.update ctx str)

-- | Turn a logger of lines into a LogFunc
lineLogger :: (BS.ByteString -> IO ()) -> IO LogFunc
lineLogger fn = do
  ref <- newIORef BS.empty
  return $ \bs -> do
    if BS.null bs
      then do
        fn =<< readIORef ref
      else do
        head <- readIORef ref
        let a:rest = BS.split '\n' bs
        left <- foldM (\a -> (fn a $>)) (head `BS.append` a) rest
        writeIORef ref $! left

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
