{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
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

- Move logging to its own module
- Improve interface for inputs to the CmdOptions
- Add support for file-trees (folders)
- Add support for JSON

* Ideas

Different types of inputs.

Lists, Trees, Graphs.

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

    , ReducerOptions (..)
    , parseReducerOptions
    , reduce

  -- | Re-export Control.Reduce for convenience:
    , L.SimpleLogger (..)
    , L.defaultLogger
    , L.LogLevel (..)
    , parseSimpleLogger

    , module Control.Reduce
    ) where

-- optparse-applicative
import           Options.Applicative

-- typed-process
import           System.Process.Typed

-- filepath
import           System.FilePath

-- text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy.Builder as Builder

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
import           Data.Foldable
import           Data.Char (toLower)
import qualified Data.List as List
import           System.Exit
import           Text.Printf

-- process
import           System.Process                        (showCommandForUser)

-- reduce-util
import           System.Process.Consume
import           Control.Logger as L

-- reduce
import           Control.Reduce
import           Data.Functor.Contravariant.PredicateM

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
  CmdOptions a
  -> a
  -> IO (ProcessConfig () () ())
toProcessConfig CmdOptions {..} =
  case inputFormat of
    ArgsInput -> \a ->
      mkProc cmd (args ++ [a])
    StreamInput -> \a ->
      setStdinLog a =<< mkProc cmd args

  where
    mkProc c' a'= do
      writeFile "cmd" $ showCommandForUser c' a'
      return $ proc cmd args

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
  (tp, p) <- timedPhase "setup" . liftIO $
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
      => Builder.Builder -> m (Logger BS.ByteString)
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
    getOptions input useStream tl c args = do
      if useStream
        then do
        s <- mkCmdOptions (StreamInput) tl c args
        case input of
          FromFile fp -> do
            rf <- BLC.fromStrict <$> BS.readFile fp
            return $ StreamOptions rf s
          FromString str' ->
            return $ StreamOptions (BLC.pack str') s
        else do
        s <- mkCmdOptions (ArgsInput) tl c args
        case input of
          FromFile fp -> do
            return $ ArgumentOptions fp s
          FromString str' ->
            return $ ArgumentOptions str' s



data CheckOptions = CheckOptions
  { expectedStatus :: ExitCode
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

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess     = 0
exitCodeToInt (ExitFailure n) = n

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
    mklogger logLevel depth =
      defaultLogger { logLevel = logLevel, maxDepth = depth }

    parseLogLevel lvl quiet =
      boundedToEnum (1 - lvl + quiet)


boundedToEnum :: (Bounded a, Enum a) => Int -> a
boundedToEnum i =
  maybe maxBound id
  . fmap fst
  . List.uncons
  . drop i
  $ enumFrom minBound
