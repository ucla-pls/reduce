{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ViewPatterns        #-}
{-|
Module      : Control.Reduce.Util
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module provides utils, so that it is easier to write command-line reducers.

* TODO

- Improve interface for inputs to the CmdOptions
- Add support for file-trees (folders)
- Add support for JSON

* Ideas

Different types of inputs.

Lists, Trees, Graphs.

-}
module Control.Reduce.Util
  ( CmdOptions (..)

  , InputFormat (..)
  , mkCmdOptions
  , toProcessConfig
  , runCmd

  , CheckOptions (..)
  , toPredicateM

  , ReducerOptions (..)
  , ReducerName (..)
  , reduce

  , exitCodeFromInt
  , exitCodeToInt

  , FileContent (..)
  , writeContent
  ) where

-- typed-process
import           System.Process.Typed

-- filepath
import           System.FilePath

-- text
import qualified Data.Text.Lazy.Builder                as Builder
import qualified Data.Text.Lazy.Encoding               as Text

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- bytestring
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.ByteString.Lazy.Char8            as BLC

-- mtl
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

-- base
import           System.Exit
import           Text.Printf

-- directory
import System.Directory (createFileLink)

-- directory-tree
import           System.Directory.Tree

-- process
import           System.Process                        (showCommandForUser)

-- reduce-util
import           Control.Reduce.Util.Logger            as L
import           System.Process.Consume

-- reduce
import           Control.Reduce
import           Data.Functor.Contravariant.PredicateM

-- checkConfig :: ! CheckConfig

data FileContent
  = Content BL.ByteString
  | SameAs FilePath
  deriving (Show, Eq)

writeContent :: FilePath -> FileContent -> IO ()
writeContent fp = \case
  Content bs ->
    BL.writeFile fp bs
  SameAs old ->
    createFileLink old fp

-- | InputFormat describe ways to interact with the command.
data InputFormat a where
  ArgsInput   :: InputFormat String
  StreamInput :: InputFormat BL.ByteString
  FileInput   :: FilePath -> InputFormat BL.ByteString
  DirInput :: FilePath -> InputFormat (DirTree FileContent)
deriving instance Show (InputFormat a)

-- | CmdOptions is a data structure that holds enough information to run a
-- single command from options described from the command line.
data CmdOptions a = CmdOptions
  { inputFormat :: ! (InputFormat a)
  , timelimit   :: ! Double
  , cmd         :: ! FilePath
  , args        :: ! [String]
  } deriving (Show)

mkCmdOptions :: InputFormat a -> Double -> FilePath -> [String] -> IO (CmdOptions a)
mkCmdOptions ifmt tl fp args = do
  cfp <- getExecutable fp
  return $ CmdOptions
    { inputFormat = ifmt
    , timelimit = tl
    , cmd = cfp
    , args = args
    }
  where
    getExecutable exec = do
      findExecutable exec >>=
        maybe (canonicalizePath exec) return

toProcessConfig ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => CmdOptions a
  -> a
  -> m (ProcessConfig () () ())
toProcessConfig CmdOptions {..} =
  case inputFormat of
    ArgsInput -> \a ->
      liftIO $ mkProc (args ++ [a])
    StreamInput -> \a ->
      liftIO $ setStdinLog a =<< mkProc args
    FileInput fn -> \a ->
      liftIO $ do
        BL.writeFile fn a
        mkProc (args ++ [fn])
    DirInput fn -> \a -> do
      liftIO $ do
        writeTreeWith writeContent (fn :/ a)
        mkProc (args ++ [fn])

  where
    mkProc args'= do
      writeFile "cmd" $ showCommandForUser cmd args'
      return $ proc cmd args'

    setStdinLog a p = do
      BLC.writeFile "stdin" $ a
      appendFile "cmd" " <stdin"
      return $ setStdin (byteStringInput a) p

runCmd ::
 (HasLogger env, MonadReader env m, MonadIO m)
  => CmdOptions a
  -> a
  -> m (Maybe (ExitCode, Sha256, Sha256))
runCmd options a = do
  (tp, p) <- timedPhase "setup" $
    toProcessConfig options a
  (tm, m) <- timedPhase "run" $ do
    olog <- getLogger "+"
    elog <- getLogger "-"
    liftIO . withFile "stdout" WriteMode $
      \hout ->
        withFile "stderr" WriteMode $
        \herr ->
          runCommandInTimelimit $
          consumeWithHash
            (combineConsumers olog $ handlerLogger hout)
            (combineConsumers elog $ handlerLogger herr)
            p
  case m of
    Just (ec, (_, ho@(showHash-> hos, olen)), (_, he@(showHash -> hes, elen))) -> do
      liftIO $ appendFile "process.csv" $
        printf "%.3f,%.3f,%d,%s,%d,%s,%d\n" tp tm (exitCodeToInt ec) hos olen hes elen
      L.info $ "exitcode:" <-> displayf "%3d" (exitCodeToInt ec)
      L.info $ "stdout" <-> displayf "(bytes: %05d):" olen
        <-> Builder.fromString (take 8 hos )
      L.info $ "stderr" <-> displayf "(bytes: %05d):" elen
        <-> Builder.fromString (take 8 hes)
      return $ Just (ec, ho, he)
    Nothing -> do
      L.warn $ "timeout"
      liftIO $appendFile "process.csv" $ "N/A,N/A,N/A,N/A\n"
      return Nothing

  where
    getLogger ::
      (HasLogger env, MonadReader env m, MonadIO m)
      => Builder.Builder -> m (Consumer BS.ByteString ())
    getLogger name = do
      env <- ask
      liftIO . perLine . logger $ maybe (return ()) (x env)
      where
        x env bs =
          runReaderT (L.log L.DEBUG (name <-> bsToBuilder bs)) env

        bsToBuilder =
          Builder.fromLazyText . Text.decodeUtf8 . BLC.fromStrict


    showHash :: BS.ByteString -> String
    showHash = concatMap (printf "%02x") . BS.unpack

    runCommandInTimelimit =
      (if timelimit options > 0 then timeout (ceiling $ timelimit options * 1e6) else fmap Just)



data CheckOptions = CheckOptions
  { expectedStatus :: ExitCode
  , preserveStdout :: Bool
  , preserveStderr :: Bool
  } deriving (Show, Eq)


-- | Creates a predicate from the CheckOptions and CmdOptions.
toPredicateM ::
 (HasLogger env, MonadReader env m, MonadUnliftIO m)
 => CheckOptions
 -> CmdOptions a
 -> FilePath
 -> a
 -> m (Maybe (PredicateM m a))
toPredicateM CheckOptions {..} cmd workFolder a = do
  phase "Initial run" $ do
    let initial = workFolder </> "initial"
    liftIO $ createDirectoryIfMissing True initial
    withCurrentDirectory initial (runCmd cmd a) >>= \case
      Just (ec, oh, eh)
        | ec /= expectedStatus ->
          return $ Nothing
        | otherwise ->
          return . Just $
            (runCmd cmd >=> testM oh eh) `contramapM` yes
      Nothing ->
        return $ Nothing
  where
    testM oh eh x = do
      let p = testp oh eh x
      L.info $ if p then "success" else "failure"
      return p

    testp oh eh = \case
      Just (ec', oh', eh') ->
        ec' == expectedStatus
        && (not preserveStdout || oh' == oh)
        && (not preserveStderr || eh' == eh)
      Nothing ->
        False

data ReducerOptions = ReducerOptions
  { reducer        :: ReducerName
  , workFolder     :: FilePath
  , keepIterations :: Bool
  } deriving (Show)

data ReducerName
  = Ddmin
  | Linear
  | Binary
  deriving (Show)


-- | Reduce using the reducer options.
reduce ::
  (HasLogger env, MonadReader env m, MonadUnliftIO m)
  => ReducerOptions
  -> String
  -> PredicateM m [a]
  -> [a]
  -> m (Maybe [a])
reduce ReducerOptions {..} name p ls = do
  phase ("Reduction" <-> Builder.fromString name) $ do
    ref <- liftIO $ newIORef (0 :: Int)
    runReducer (mmap (logComputation ref) p)
  where
    logComputation ::
      (HasLogger env, MonadReader env m, MonadUnliftIO m)
      => IORef Int
      -> m a
      -> m a
    logComputation ref ma = do
      (x, folder) <- liftIO $ do
        x <- atomicModifyIORef ref (\a -> (succ a, a))
        let folder = workFolder </> printf "%s-%04d" name x
        createDirectoryIfMissing True folder
        return (x, folder)
      phase ("Iteration" <-> displayf "%04d" x) $
        withCurrentDirectory folder $
          ma

    runReducer p' =
      case reducer of
        Ddmin ->
          unsafeDdmin p' ls
        Linear ->
          runMaybeT (unsafeLinearReduction (asMaybeGuard p') ls)
        Binary ->
          binaryReduction p' ls


exitCodeFromInt :: Int -> ExitCode
exitCodeFromInt = \case
  0 -> ExitSuccess
  n -> ExitFailure n

exitCodeToInt :: ExitCode -> Int
exitCodeToInt = \case
  ExitSuccess -> 0
  ExitFailure n -> n
