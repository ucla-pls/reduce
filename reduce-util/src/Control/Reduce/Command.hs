{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Control.Reduce.Command
Description : A module to define commands
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module focuses on an easy to use interface for communicating
with command line predicates.

-}

module Control.Reduce.Command
  (
  -- * Command
  -- | Command is a definition of a reproducible command in a system.

  runCommand

  , setupCommand
  , CmdInput (..)
  , emptyInput
  , inputFromDirTree
  , CmdResult (..)
  , CmdOutputSummary (..)

  -- * Command Template
  -- |
  --
  -- @
  -- run %file.txt {}
  -- @
  --
  -- - % explains that the argument is a file, and we should consider the
  --   absolute path when running the program.
  -- - {} is the input to the predicate.

  , CmdTemplate (..)
  , createCmdTemplate

  , CmdArgument (..)

  , parseCmdArgument

  -- ** Helpers
  , canonicalizeOrFail
  , evaluateTemplate
  , evaluateArgument
  , templateToString
  , argumentToString
  , replaceRelative

  -- * Utils
  , showHash
  -- , replaceRelative
  , exitCodeFromInt
  , exitCodeToInt

  , ExitCode (..)
  , Sha256
  ) where


-- typed-process
import           System.Process.Typed

-- filepath
import           System.FilePath

-- text
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Encoding    as Text
import qualified Data.Text.Lazy.IO          as LazyText

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- bytestring
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

-- mtl
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer

-- containers
import qualified Data.Map                   as Map

-- base
import           Data.Bifunctor             (first)
import           Data.Foldable
import           Data.Functor
import qualified Data.List                  as L
import           Data.String
import           Data.Void
import           System.Exit
import           System.IO.Error
import           Text.Printf

-- megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- dirtree
import           System.DirTree

-- reduce-util
import           Control.Reduce.Util.Logger as L
import           System.Process.Consume


-- | Run a command in a way that makes the results reproducable. This
-- method stores the command and the results in the workfolder.
runCommand ::
  (L.HasLogger env, MonadReader env m, MonadUnliftIO m)
  => FilePath
  -- ^ the workfolder to setup the command in.
  -> Double
  -- ^ the timelimit
  -> CmdTemplate
  -- ^ the template to describe the command.
  -> CmdInput
  -- ^ the input to the command.
  -> m (CmdResult (FilePath, Maybe CmdOutputSummary))
  -- ^ A logged results of the command.
runCommand workDir cmdTimelimit cmdTemplate cmdInput = do
  (tp, pr) <- timedPhase "setup" $ do
    fn <- liftIO $ setupCommand workDir cmdTemplate cmdInput
    return $ proc fn []
  (tm, x) <- timedPhase "run" $ do
    olog <- traceLogger "+"
    elog <- traceLogger "-"
    liftIO . withFile "stdout" WriteMode $ \hout ->
      withFile "stderr" WriteMode $ \herr ->
      tryTimeout cmdTimelimit $
      consumeWithHash
      (combineConsumers olog $ handlerLogger hout)
      (combineConsumers elog $ handlerLogger herr)
      pr
  let m = formatOutput <$> x
  logResults m
  return (CmdResult tp tm (workDir </> "sandbox", m))
  where
    formatOutput (ec, (_, ho), (_, he)) =
      CmdOutputSummary ec ho he

    traceLogger name = do
      env <- ask
      liftIO . perLine . logger $ maybe (return ()) (x env)
      where
        x env bs =
          runReaderT (L.log L.TRACE (name <-> bsToBuilder bs)) env
        bsToBuilder =
          Builder.fromLazyText . Text.decodeUtf8 . BLC.fromStrict

    logResults = \case
      Just (CmdOutputSummary ec ho he) -> do
        L.debug $ "exitcode:" <-> displayf "%3d" (exitCodeToInt ec)
        L.debug $ "stdout" <-> displayf "(bytes: %05d):" (snd ho)
          <-> Builder.fromString (take 8 (showHash ho))
        L.debug $ "stderr" <-> displayf "(bytes: %05d):" (snd he)
          <-> Builder.fromString (take 8 (showHash he))
      Nothing ->
        L.warn "Process Timed-out"

-- | A command result
data CmdResult a = CmdResult
  { resultSetupTime :: !Double
  , resultRunTime   :: !Double
  , resultOutput    :: !a
  } deriving (Show, Eq, Ord, Functor)

-- | A summarized output of a command.
data CmdOutputSummary = CmdOutputSummary
  { outputCode :: !ExitCode
  , outputOut  :: !Sha256
  , outputErr  :: !Sha256
  } deriving (Show, Eq, Ord)

-- | A command template. These templates contains information about how to run
-- a command given an `CmdInput`.
data CmdTemplate = CmdTemplate
  { cmdTimelimit :: !Double
  , cmdProgram   :: !FilePath
  , cmdArguments :: ![CmdArgument]
  } deriving (Show, Eq)

-- | A command input
data CmdInput = CmdInput
  { ciValueMap         :: ! (Map.Map String CmdArgument)
  -- ^ Values to be replaced in the argument (see `CmdTemplate`)
  , ciStdin            :: ! (Maybe BL.ByteString)
  -- ^ A bytestring to use as standard input to the command.
  , ciDirectoryContent :: ! (Maybe (RelativeDirForest Link BL.ByteString))
  -- ^ A `DirTree` of files to be in the working directory of the command.
  } deriving (Show, Eq)

-- | Creates an empty input.
emptyInput :: CmdInput
emptyInput = CmdInput Map.empty Nothing Nothing

-- | Creates an input from a 'DirTree' given a name of the file.
-- This function also sets up the key '{}' to point to the dirtree
inputFromDirTree :: String -> RelativeDirTree Link BL.ByteString -> CmdInput
inputFromDirTree fileName dirtree =
  emptyInput
  { ciValueMap = Map.singleton "" (CAJoin (CAConst "input") (CAConst fileName))
  , ciDirectoryContent =
      Just $ singletonForest fileName dirtree
  }

-- | Creates a reporducable command in a workding directory of choice.
-- returns the absolute path of the command to run.
setupCommand ::
  FilePath
  -> CmdTemplate
  -> CmdInput
  -> IO FilePath
setupCommand workDir cmdTemplate CmdInput{..} = do
  createDirectory workDir
  withCurrentDirectory workDir $ do
    mapM_ (BLC.writeFile "stdin") ciStdin
    mapM_ (writeRelativeDirTree BLC.writeFile "input" . directory) ciDirectoryContent
    writeExec "run.sh" (Builder.toLazyText . execWriter $ shellScript)
    makeAbsolute "run.sh"
  where
    writeExec fp txt = do
      LazyText.writeFile fp txt
      getPermissions fp >>=
        setPermissions fp. setOwnerExecutable True

    shellScript = do
      tell "#!/usr/bin/env sh\n"
      tell "WORKDIR=${2:-$(pwd)}\n"
      tell "SANDBOX=${1:-\"$WORKDIR/sandbox\"}\n"
      tell "mkdir -p \"$SANDBOX\"\n"
      tell "cd \"$SANDBOX\"\n"
      tell $ templateToString cmdTemplate (workDir, "$WORKDIR") ciValueMap
      case ciStdin of
        Just _ ->
          tell " < \"$WORKDIR/stdin\""
        Nothing -> return ()
      tell "\n"


-- | Command line arguments, can either be a constant string
-- an absolute filepath or an input.
data CmdArgument
  = CAConst !String
  | CAFilePath !FilePath
  | CAInput !String
  | CAJoin CmdArgument CmdArgument
  deriving (Show, Eq)

instance Semigroup CmdArgument where
  (<>) (CAConst a) (CAConst b) = CAConst (a <> b)
  (<>) a b                     = CAJoin a b

-- | Creates a new command from string arguments. Can fail if the executable or
-- any of the files mentioned in the arguments does not exits, or
-- if it can't parse the arguments.
createCmdTemplate :: Double -> String -> [String] -> IO (Either String CmdTemplate)
createCmdTemplate timelimit cmd args = runExceptT $ do
  exactCmd <- tryL $ getExecutable cmd
  results <-
    mapM (tryL . canonicalizeArgument)
    =<< mapM (liftEither . parseCmdArgument) args
  return $ CmdTemplate timelimit exactCmd results
  where
    getExecutable exec =
      findExecutable exec >>=
        maybe (canonicalizeOrFail exec) return

    tryL :: IO a -> ExceptT String IO a
    tryL m = do
      x <- liftIO $ UnliftIO.tryIO m
      liftEither (first show x)

-- | Creates a string which can be embedded in a script or run directly on the
-- command line.
templateToString ::
  (Monoid m, IsString m)
  => CmdTemplate
  -> (FilePath, String)
  -> Map.Map String CmdArgument
  -> m
templateToString cmd fp kmap =
  fold
  . L.intersperse (fromString " ")
  . map (\x -> "\"" <> x <> "\"")
  $ fromString a : ms
  where
    (a, ms) = evaluateTemplate cmd fp kmap

-- | Evalutates a `Cmd` with a keymap, so that the results can be
-- used to create a processs.
evaluateTemplate ::
  (Semigroup m, IsString m)
  => CmdTemplate
  -> (FilePath, String)
  -> Map.Map String CmdArgument
  -> (String, [m])
evaluateTemplate (CmdTemplate _ str args) fp kmap =
  ( replaceRelative str fp
  , map ( argumentToString
         . flip relativeArgument fp
         . flip evaluateArgument kmap
        ) args
  )

parseCmdArgument :: String -> Either String CmdArgument
parseCmdArgument arg =
  parsePretty ((foldr1 (<>) <$> many cliArgumentP) <* eof) arg arg
  where
    cliArgumentP :: Parsec Void String CmdArgument
    cliArgumentP =
      msum
      [ CAFilePath <$> (char '%' *> takeWhile1P Nothing (const True))
      , CAConst <$> (string "}}" $> "}")
      , do
          _ <- char '{'
          msum
            [ CAConst . (:[]) <$> char '{'
            , CAFilePath <$> (char '%' *> takeWhileP Nothing (/= '}') <* char '}')
            , CAInput <$> takeWhileP Nothing (/= '}') <* char '}'
            ]
      , CAConst <$> some (satisfy (\x -> x /= '{' && x /= '}'))
      ]

-- | Returns the arguments with all the file pointers
-- canonicalized. Throws an error if the file cannot be found.
-- See `canonicalizeOrFail`
canonicalizeArgument :: CmdArgument -> IO CmdArgument
canonicalizeArgument = \case
  CAJoin a b ->
    CAJoin
    <$> canonicalizeArgument a
    <*> canonicalizeArgument b
  CAFilePath fp ->
    CAFilePath <$> canonicalizeOrFail fp
  a ->
    return a

-- | Returns the filepath with relative filepath replaced by a string.
replaceRelative :: FilePath -> (FilePath, String) -> FilePath
replaceRelative fp (path, key) =
  key </> makeRelative path fp

-- | Returns the arguments, but with all the relative filepath to the
-- replaced by a keyword.
relativeArgument :: CmdArgument -> (FilePath, String) -> CmdArgument
relativeArgument (CAJoin a b) =
  CAJoin
  <$> relativeArgument a
  <*> relativeArgument b
relativeArgument (CAFilePath fp) =
  CAFilePath <$> replaceRelative fp
relativeArgument a =
  return a

-- | Canonicalize a file path and fail if it does not exist.
-- Throws an IOException with the doesNotExistErrorType if
-- it cannot be found.
canonicalizeOrFail :: FilePath -> IO FilePath
canonicalizeOrFail fp = do
  cfp <- canonicalizePath fp
  x <- doesPathExist cfp
  unless x . ioError
    $ mkIOError
      doesNotExistErrorType
      "Expected file"
      Nothing
      (Just cfp)
  return cfp

-- | Evaluate an argument using a map from keys to strings.
evaluateArgument :: CmdArgument -> Map.Map String CmdArgument -> CmdArgument
evaluateArgument ca =
  case ca of
    CAConst _ -> const ca
    CAFilePath _ -> const ca
    CAInput key -> Map.findWithDefault ca key
    CAJoin a b ->
      CAJoin <$> evaluateArgument a <*> evaluateArgument b

argumentToString :: (IsString a, Semigroup a) => CmdArgument -> a
argumentToString = \case
  CAConst str -> fromString str
  CAFilePath fp -> fromString fp
  CAInput key -> "{" <> fromString key <> "}"
  CAJoin a b ->
    argumentToString a <> argumentToString b

tryTimeout :: (MonadUnliftIO m, RealFrac r) => r -> m a -> m (Maybe a)
tryTimeout timelimit =
  if timelimit > 0
  then timeout (ceiling $ timelimit * 1e6)
  else fmap Just

-- | Converts a hash to a hexstring
showHash :: Sha256 -> String
showHash =
  concatMap (printf "%02x") . BS.unpack . fst

-- | Convert an `Int` to an `ExitCode`.
exitCodeFromInt :: Int -> ExitCode
exitCodeFromInt = \case
  0 -> ExitSuccess
  n -> ExitFailure n

-- | Convert an `ExitCode` to an `Int`.
exitCodeToInt :: ExitCode -> Int
exitCodeToInt = \case
  ExitSuccess -> 0
  ExitFailure n -> n

parsePretty :: Parsec Void String a -> String -> String -> Either String a
parsePretty parser name bs =
#if MIN_VERSION_megaparsec(7,0,0)
  first errorBundlePretty $ parse parser name bs
#else
  first parseErrorPretty $ parse parser name bs
#endif
