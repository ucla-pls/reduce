{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
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
- Make output csv file
- Make understandable logging.

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

  -- , mkCmdOptionsPredicate
  -- , parseCmdOptions

  -- , Check (..)

  -- , consume
  -- | Re-export Control.Reduce for convenience:
  , module Control.Reduce
  ) where

-- optparse-applicative
import           Options.Applicative

-- typed-process
import           System.Process.Typed

-- filepath
import           System.FilePath

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- time
import           Data.Time.Clock (getCurrentTime, diffUTCTime)

-- bytestring
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.ByteString.Lazy.Char8            as BLC

-- cryptohash-sha256
import           Crypto.Hash.SHA256                    as SHA256

-- stm
import           Control.Concurrent.STM

-- mtl
import           Control.Monad.Reader
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Maybe

-- base
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor
import           Data.Int
import           Data.Word
import           System.Exit
import           System.IO.Error
import           Text.Printf

-- process
import           System.Process                        (showCommandForUser)

-- contravariant
import           Data.Functor.Contravariant

-- reduce-util
import           System.Process.Consume

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
    mkProc cmd args = do
      writeFile "cmd" $ showCommandForUser cmd args
      return $ proc cmd args

    setStdinLog a p = do
      BLC.writeFile "stdin" $ a
      appendFile "cmd" " <stdin"
      return $ setStdin (byteStringInput a) p

runCmd ::
 (HasLoggers env, MonadReader env m, MonadIO m)
  => CmdOptions a
  -> a
  -> m (Maybe (ExitCode, Sha256, Sha256))
runCmd options a = do
  olog <- asks stdoutLog
  elog <- asks stderrLog

  liftIO $ do
    (tp, p) <- timeProcess $ toProcessConfig options a
    (tm, m) <-
      withFile "stdout" WriteMode $
      \hout ->
        withFile "stderr" WriteMode $
        \herr ->
           timeProcess . runCommandInTimelimit $
            consumeWithHash
              (combineConsumers olog $ handlerLogger hout)
              (combineConsumers elog $ handlerLogger herr)
              p
    case m of
      Just (ec, (_, ho@(showHash-> hos, olen)), (_, he@(showHash -> hes, elen))) -> do
        appendFile "process.csv" $
          printf "%.3f,%.3f%,%d,%s,%d,%s,%d\n" tp tm (exitCodeToInt ec) hos olen hes elen
        return $ Just (ec, ho, he)
      Nothing -> do
        appendFile "process.csv" $ "N/A,N/A,N/A,N/A\n"
        return Nothing

  where
    showHash :: BS.ByteString -> String
    showHash = concatMap (printf "%02x") . BS.unpack

    runCommandInTimelimit =
      (if timelimit options > 0 then timeout (ceiling $ timelimit options * 1e6) else fmap Just)

    timeProcess :: IO a -> IO (Double, a)
    timeProcess m = do
      start <- getCurrentTime
      a <- m
      end <- getCurrentTime
      return (realToFrac $ diffUTCTime end start, a)

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
   <> metavar "seconds"
   <> value 60.0
   <> showDefault
   <> help "the maximum number of seconds to run the process, negative means no timelimit.")
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
          FromString str ->
            return $ StreamOptions (BLC.pack str) s
        else do
        s <- mkCmdOptions (ArgsInput) tl c args
        case input of
          FromFile fp -> do
            return $ ArgumentOptions fp s
          FromString str ->
            return $ ArgumentOptions str s



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
 (HasLoggers env, MonadReader env m, MonadUnliftIO m)
 => CheckOptions
 -> CmdOptions a
 -> FilePath
 -> a
 -> m (Maybe (PredicateM m a))
toPredicateM CheckOptions {..} cmd workFolder a = do
  let initial = workFolder </> "initial"
  liftIO $ createDirectoryIfMissing True initial
  (withCurrentDirectory initial $ runCmd cmd a) >>= \case
    Just (ec, oh, eh)
      | ec /= expectedStatus ->
        return $ Nothing
      | otherwise ->
        return . Just $ runCmd cmd `contramapM` ( testp oh eh `contramap` yes )
    Nothing ->
      return $ Nothing
  where
    testp oh eh  = \case
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

fromString :: String -> ReducerName
fromString = \case
  "ddmin" -> Ddmin
  "linear" -> Linear
  "binary" -> Binary

parseReducerOptions :: Parser (IO ReducerOptions)
parseReducerOptions =
  mkReduceOptions
  <$> (
  fromString <$> strOption
    ( long "reducer"
      <> short 'R'
      <> help "the reducing algorithm to use."
      <> value "binary"
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
  (HasLoggers env, MonadReader env m, MonadUnliftIO m)
  => ReducerOptions
  -> String
  -> PredicateM m [a]
  -> [a]
  -> m (Maybe [a])
reduce ReducerOptions {..} name pred ls = do
  ref <- liftIO $ do
    newIORef (0 :: Int)
  getReducer (mmap (logComputation ref) pred) ls
  where
    sandbox ::
      (HasLoggers env, MonadReader env m, MonadUnliftIO m)
      => FilePath
      -> m a
      -> m a
    sandbox folder ma = do
      let sbx = folder </> "sandbox"
      liftIO $ createDirectoryIfMissing True sbx
      withCurrentDirectory sbx ma

    logComputation ::
      (HasLoggers env, MonadReader env m, MonadUnliftIO m)
      => IORef Int
      -> m a
      -> m a
    logComputation ref ma = do
      folder <- liftIO $ do
        x <- atomicModifyIORef ref (\a -> (succ a, a))
        let folder = workFolder </> printf "%s-%04d" name x
        createDirectoryIfMissing True folder
        return folder

      sandbox folder ma

    getReducer pred ls =
      case reducer of
        Ddmin ->
          unsafeDdmin pred ls
        Linear ->
          runMaybeT (unsafeLinearReduction (asMaybeGuard pred) ls)
        Binary ->
          binaryReduction pred ls
