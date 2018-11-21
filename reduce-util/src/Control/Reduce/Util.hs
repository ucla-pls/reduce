{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE GADTs #-}
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

    CmdOptions (..)
    , mkCmdOptions
    , toProcessConfig
    , runCmd

    , CmdOptionWithInput (..)
    , parseCmdOptionsWithInput

    , CheckOptions
    , parseCheckOptions
    , toPredicateM

  -- , mkCmdOptionsPredicate
  -- , parseCmdOptions

  -- , Check (..)

  -- , consume
  -- | Re-export Control.Reduce for convenience:
  , module Control.Reduce
  ) where

-- optparse-applicative
import Options.Applicative

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

-- mtl
import Control.Monad.Reader.Class

-- base
import System.Exit
import System.IO.Error
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Word
import Data.Functor
import Data.Foldable
import Control.Applicative

-- contravariant
import Data.Functor.Contravariant

-- reduce-util
import System.Process.Consume

-- reduce
import Control.Reduce
import Data.Functor.Contravariant.PredicateM

-- checkConfig :: ! CheckConfig

-- | InputFormat describe ways to interact with the command.
data InputFormat a where
  ArgsInput :: InputFormat String
  StreamInput :: InputFormat BL.ByteString
deriving instance Show (InputFormat a)

-- | CmdOptions is a data structure that holds enough information to run a
-- single command from options described from the command line.
data CmdOptions a = CmdOptions
  { inputFormat :: ! (InputFormat a)
  , cmd         :: ! FilePath
  , args        :: ! [String]
  } deriving (Show)

mkCmdOptions :: InputFormat a -> FilePath -> [String] -> IO (CmdOptions a)
mkCmdOptions ifmt fp args = do
  cfp <- getExecutable fp
  return $ CmdOptions
    { inputFormat = ifmt
    , cmd = cfp
    , args = args
    }
  where
    getExecutable exec = do
      findExecutable exec >>=
        maybe (canonicalizePath exec) return

toProcessConfig ::
  CmdOptions a
  -> a
  -> IO (ProcessConfig () () ())
toProcessConfig CmdOptions {..} =
  case inputFormat of
    ArgsInput -> \a ->
      return $ proc cmd (args ++ [a])
    StreamInput -> \a ->
      return $ setStdin (byteStringInput a) $ proc cmd args

runCmd ::
 (HasLoggers env, MonadReader env m, MonadIO m)
  => CmdOptions a
  -> a
  -> m (ExitCode, Sha256, Sha256)
runCmd options a = do
  logWithHash =<< (liftIO $ toProcessConfig options a)

data CmdOptionWithInput
  = ArgumentOptions !String !(CmdOptions String)
  | StreamOptions !BL.ByteString !(CmdOptions BL.ByteString)
  deriving (Show)

data InputFrom
  = FromString String
  | FromFile FilePath
  deriving (Show, Eq)

parseCmdOptionsWithInput :: Parser (IO CmdOptionWithInput)
parseCmdOptionsWithInput =
  getOptions
  <$> ( asum
       [ FromString <$> strOption (long "input" <> short 'i' <> help "the input to code")
       , FromFile <$> strOption (long "file" <> short 'f' <> help "read input from file or folder")
       ])
  <*> flag False True (long "stream" <> short 'S' <> help "stream the input to the process")
  <*> strArgument (metavar "CMD" <> help "the command to run")
  <*> many (strArgument (metavar "ARG.." <> help "arguments to the command"))
  where
    getOptions input useStream c args = do
      if useStream
        then do
        s <- mkCmdOptions (StreamInput) c args
        case input of
          FromFile fp -> do
            rf <- BLC.fromStrict <$> BS.readFile fp
            return $ StreamOptions rf s
          FromString str ->
            return $ StreamOptions (BLC.pack str) s
        else do
        s <- mkCmdOptions (ArgsInput) c args
        case input of
          FromFile fp -> do
            return $ ArgumentOptions fp s
          FromString str ->
            return $ ArgumentOptions str s



data CheckOptions = CheckOptions
  { expectedStatus   :: ExitCode
  , preserveStdout :: Bool
  , preserveStderr :: Bool
  } deriving (Show, Eq)

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
  where
    exitCodeFromInt :: Int -> ExitCode
    exitCodeFromInt 0 = ExitSuccess
    exitCodeFromInt n = ExitFailure n

-- | Creates a predicate from the CheckOptions and CmdOptions.
toPredicateM ::
 (HasLoggers env, MonadReader env m, MonadIO m)
 => CheckOptions
 -> CmdOptions a
 -> a
 -> m (Maybe (PredicateM m a))
toPredicateM CheckOptions {..} cmd a = do
  (ec, oh, eh) <- runCmd cmd a
  if ec /= expectedStatus
    then
    return Nothing
    else
    return $ Just (runCmd cmd `contramapM` ( testp oh eh `contramap` ifTrueT ))
  where
    testp oh eh (ec', oh', eh') =
      ec' == expectedStatus
      && (not preserveStderr || oh' == oh)
      && (not preserveStderr || eh' == eh)


-- doStream :: Bool -> InputFormat a
-- doStream True = StreamInput
-- doStream False = ArgsInput




-- -- data Check
-- --   = Status ExitCode
-- --   | StdOutHash SHA256
-- --   | StdErrHash SHA256
-- --   deriving (Show, Eq)

-- -- -- | We can create a 'FilePathPredicate' from a command.
-- -- mkCmdOptionsPredicate :: CmdOptions a -> IO (PredicateM IO a)
-- -- mkCmdOptionsPredicate CmdOptions {..} = do
-- --   return ( resolve `contramap` testCmd [Status ExitSuccess] )
-- --   where
-- --     resolve =
-- --       case inputFormat of
-- --         ArgsInput ->
-- --           \a -> proc cmd (args ++ a)
-- --         StreamInput ->
-- --           \a -> setStdin (byteStringInput a) $ proc cmd args

-- -- mkCmdPredicate :: (a -> IO (ProcessConfig () () ())) -> [Check] -> PredicateM IO a
-- -- mkCmdPredicate fn test  = do
-- --   contramapM fn (testCmd test)

-- -- testCmd :: [Check] -> PredicateM IO (ProcessConfig () () ())
-- -- testCmd test =
-- --   consumeWithHash ignoreConsumer ignoreConsumer
-- --   `contramapM` contramap testp ifTrueT
-- --   where
-- --     testp (ec, ((), out), ((), err)) =
-- --       flip all test $ \case
-- --         Status ec' -> ec == ec'
-- --         StdOutHash hash -> hash == out
-- --         StdErrHash hash -> hash == err


-- data CheckConfig = CheckConfig
--   { expectStatus   :: ExitCode
--   , perserveStdout :: Bool
--   , perserveStderr :: Bool
--   , checkCheck :: Bool
--   } deriving (Show, Eq)

-- parseCmdOptions :: Parser (IO (CmdOptions a))
-- parseCmdOptions =
--   getOptions
--   <$> ( CheckConfig
--         <$> ( exitCodeFromInt
--               <$> option auto
--               ( long "exit-code"
--                 <> short 'E'
--                 <> help "preserve exit-code"
--                 <> value 0
--                 <> metavar "CODE"
--                 <> showDefault)
--             )
--         <*> flag False True (long "stdout" <> help "preserve stdout.")
--         <*> flag False True (long "stderr" <> help "preserve stderr.")
--         <*> flag True False (long "no-check" <> help "don't check the property on the initial input.")
--       )
--   <*> flag False True (long "stream" <> help "stream the input")
--   <*> strArgument (metavar "CMD" <> help "the command to run")
--   <*> many (strArgument (metavar "ARG.." <> help "arguments to the command"))
--   where
--     exitCodeFromInt :: Int -> ExitCode
--     exitCodeFromInt 0 = ExitSuccess
--     exitCodeFromInt n = ExitFailure n

--     getOptions x cc c args = do
--       mkCmdOptions x cc c args

-- -- -- | The options to build a command-line reducer.
-- -- data ReducerOptions a = ReducerOptions
-- --   {
-- --     -- | The underlying reducer to be used to reduce the item.
-- --     reducer :: Reducer IO a

-- --     -- | Cmd Line Options
-- --   , cliOptions :: CmdOptions
-- --   }

-- -- -- | Runs the reducer is the IO monad
-- -- reduce :: ReducerOptions FilePath -> a -> IO (Maybe a)
-- -- reduce ReducerOptions {..} a = do
-- --   absoluteCmd <- canonicalizePath command
-- --   ref <- newIORef (0 :: Int)
-- --   reducer ( predicate absoluteCmd ref ) a
-- --   where
-- --     predicate absoluteCmd ref simpl = do
-- --       i <- atomicModifyIORef' ref (\i -> ( succ i, i))

-- --       let folder = workFolder </> printf "%04o" i

-- --       createDirectoryIfMissing True folder

-- --       success <- withCurrentDirectory folder $ do
-- --         file <- toFile simpl
-- --         (ec, _, _ ) <- readProcess ( proc absoluteCmd ( args ++ [file] ) )
-- --         return (ec == ExitSuccess)

-- --       when
-- --         ( not $ keepIterations )
-- --         ( removePathForcibly folder )

-- --       return success
