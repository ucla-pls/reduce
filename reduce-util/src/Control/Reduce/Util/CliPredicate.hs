{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Control.Reduce.Util.CliPredicate
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module focuses on an easy to use interface for communicating
with command line predicates.

Examples:

@
run %file.txt {}
@

- % explains that the argument is a file, and we should consider the
  absolute path when running the program.
- {} is the input to the predicate.


Todo:

Maybe have an input DSL? Ideally a Semitic for creating the workfolder.

-}

module Control.Reduce.Util.CliPredicate
  (
    Command (..)
  , makeCommand
  , runCommand
  , runCommandWithLogger
  , setup
  , postprocess
  , createCommand

  , CommandTemplate (..)
  , createCommandTemplate
  , evaluateTemplate
  , templateToString

  , CmdInput (..)
  , inputArgument
  , inputStream
  , inputFile
  , inputDirectory
  , inputDirectoryWith
  , inputDirTree
  , inputDirTreeWith

  , CmdOutput (..)

  , CmdResult (..)
  , resultExitCode
  , resultStdout
  , resultStderr

  , CmdArgument (..)
  , parseCmdArgument
  , evaluateArgument
  , argumentToString
  , canonicalizeArgument

  , relativeArgument
  , canonicalizeOrFail

  -- * Utils
  , showHash
  , replaceRelative
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
import qualified Data.Text.Lazy             as LazyText
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

-- lens
import           Control.Lens

-- containers
import qualified Data.Map                   as Map

-- base
import           Data.Bifunctor             (first)
import           Data.Functor
import           Data.Foldable
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


-- | A representation of a command-line command.
data Command a b = Command
  { cmdTemplate    :: !CommandTemplate
  , cmdSetup       :: a -> CmdInput
  , cmdPostprocess :: Maybe CmdOutput -> b
  }

instance Profunctor Command where
  dimap ab cb (Command t s p) =
    Command t (s . ab) (cb . p)

-- | Add a setup action to the command
setup :: (c -> a) -> Command a b -> Command c b
setup = lmap
{-# INLINE setup #-}

-- | Add a postprocessing step to the command
postprocess :: (b -> c) -> Command a b -> Command a c
postprocess = rmap
{-# INLINE postprocess #-}

-- | Create a command from a command template
makeCommand :: CommandTemplate -> Command CmdInput (Maybe CmdOutput)
makeCommand tm =
  Command tm id id
{-# INLINE makeCommand #-}

createCommand :: Double -> String -> [String] -> IO (Either String (Command CmdInput (Maybe CmdOutput)))
createCommand d s args =
  fmap makeCommand <$> createCommandTemplate d s args
{-# INLINE createCommand #-}

-- | Run a command in a working folder. Fails if the working folder
-- already exists.
runCommand :: FilePath -> Command a b -> a -> LoggerT IO (CmdResult b)
runCommand fp cmd a =
  LoggerT . ReaderT $ runCommandWithLogger fp cmd a
{-# INLINE runCommand #-}

-- | Run the command with a logger
runCommandWithLogger :: FilePath -> Command a b -> a -> LoggerConfig -> IO (CmdResult b)
runCommandWithLogger workDir Command {..} a lg = do
  createDirectoryIfMissing True workDir
  withCurrentDirectory workDir . flip runReaderT lg $ do
    (tp, pr) <- timedPhase "setup" $
      liftIO $ runCmdInput (cmdSetup a) >>= setupProcess
    (tm, x) <- timedPhase "run" $ do
      olog <- traceLogger "+"
      elog <- traceLogger "-"
      liftIO . withFile "stdout" WriteMode $ \hout ->
        withFile "stderr" WriteMode $ \herr ->
        tryTimeout (cmdTimelimit cmdTemplate) $
        consumeWithHash
        (combineConsumers olog $ handlerLogger hout)
        (combineConsumers elog $ handlerLogger herr)
        pr
    let m = formatOutput <$> x
    logResults m
    return $ CmdResult tp tm (cmdPostprocess m)
  where
    formatOutput (ec, (_, ho), (_, he)) =
      CmdOutput ec ho he

    traceLogger name = do
      env <- ask
      liftIO . perLine . logger $ maybe (return ()) (x env)
      where
        x env bs =
          runReaderT (L.log L.TRACE (name <-> bsToBuilder bs)) env
        bsToBuilder =
          Builder.fromLazyText . Text.decodeUtf8 . BLC.fromStrict

    -- | Creates a shell script which can be executed in the current working
    -- directory.
    setupProcess :: Input -> IO (ProcessConfig () () ())
    setupProcess cmdInput@Input{..}  = do
      maybe (return ()) (BLC.writeFile "stdin") ciStdin
      writeExec "run.sh" $ toShellScript cmdInput
      return $ proc "./run.sh" []
      where
        writeExec fp txt = do
          LazyText.writeFile fp txt
          getPermissions fp >>=
            setPermissions fp. setOwnerExecutable True

    -- | Creates a shell script which can be executed in the current working
    -- directory.
    toShellScript :: Input -> LazyText.Text
    toShellScript Input{..} =
      Builder.toLazyText . execWriter $ do
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


-- | A command template. These templates contains information about how to run
-- a command given an `CmdInput`.
data CommandTemplate = CommandTemplate
  { cmdTimelimit :: !Double
  , cmdProgram   :: !FilePath
  , cmdArguments :: ![CmdArgument]
  } deriving (Show, Eq)


-- | Creates a new command from string arguments. Can fail if the executable or
-- any of the files mentioned in the arguments does not exits, or
-- if it can't parse the arguments.
createCommandTemplate :: Double -> String -> [String] -> IO (Either String CommandTemplate)
createCommandTemplate timelimit cmd args = runExceptT $ do
  exactCmd <- tryL $ getExecutable cmd
  results <-
    mapM (tryL . canonicalizeArgument)
    =<< mapM (liftEither . parseCmdArgument) args
  return $ CommandTemplate timelimit exactCmd results
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
  => CommandTemplate
  -> (FilePath, String)
  -> Map.Map String CmdArgument
  -> m
templateToString cmd fp kmap =
  let
    (a, ms) = evaluateTemplate cmd fp kmap
  in
    fold
    . L.intersperse (fromString " ")
    . map (\x -> "\"" <> x <> "\"")
    $ fromString a : ms

-- | Evalutates a `Cmd` with a keymap, so that the results can be
-- used to create a processs.
evaluateTemplate ::
  (Semigroup m, IsString m)
  => CommandTemplate
  -> (FilePath, String)
  -> Map.Map String CmdArgument
  -> (String, [m])
evaluateTemplate (CommandTemplate _ str args) fp kmap =
  ( replaceRelative str fp
  , map ( argumentToString
         . flip relativeArgument fp
         . flip evaluateArgument kmap
        ) args
  )

-- | Command line arguments, can either be a constant string
-- an absolute filepath or an input.
data CmdArgument
  = CAConst !String
  | CAFilePath !FilePath
  | CAInput !String
  | CAJoin CmdArgument CmdArgument
  deriving (Show, Eq)

instance Semigroup CmdArgument where
  (<>) (CAConst a) (CAConst b) =
    CAConst (a <> b)
  (<>) a b = CAJoin a b

parseCmdArgument :: String -> Either String CmdArgument
parseCmdArgument arg =
  first errorBundlePretty $ parse ((foldr1 (<>) <$> many cliArgumentP) <* eof) arg arg
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


newtype CmdInput = CmdInput { runCmdInput :: IO Input }

data Input = Input
  { ciValueMap :: Map.Map String CmdArgument
  , ciStdin    :: Maybe BL.ByteString
  } deriving (Show, Eq)


instance Semigroup CmdInput where
  (<>) a b = CmdInput $ (<>) <$> runCmdInput a <*> runCmdInput b

instance Monoid CmdInput where
  mempty = CmdInput $ return mempty

instance Semigroup Input where
  (<>) (Input avm asi) (Input bvm bsi) =
    Input (avm <> bvm) (asi <> bsi)

instance Monoid Input where
  mempty = Input mempty mempty

inputArgument ::
  String
  -> CmdInput
inputArgument str = CmdInput $
  return $ mempty { ciValueMap = Map.singleton "" (CAConst str) }

inputStream ::
  BL.ByteString
  -> CmdInput
inputStream bs = CmdInput $
  return $ mempty { ciStdin = Just bs }

inputDirTree ::
  String
  -> DirTree Link BL.ByteString
  -> CmdInput
inputDirTree =
  inputDirTreeWith BL.writeFile

inputDirTreeWith ::
  (FilePath -> a -> IO ())
  -> String
  -> DirTree Link a
  -> CmdInput
inputDirTreeWith f name dt = CmdInput $ do
  name' <- liftIO $ do
    writeDirTree f name dt
    makeAbsolute name
  return $ mempty { ciValueMap = Map.singleton "" (CAFilePath name')}

inputFile ::
  String
  -> BL.ByteString
  -> CmdInput
inputFile name = inputDirTree name . file

inputDirectory ::
  String
  -> FileMap (DirTree Link BL.ByteString)
  -> CmdInput
inputDirectory =
  inputDirectoryWith BL.writeFile

inputDirectoryWith ::
  (FilePath -> a -> IO ())
  -> String
  -> FileMap (DirTree Link a)
  -> CmdInput
inputDirectoryWith f name = inputDirTreeWith f name . directory

-- inputDirTree ::
--   String
--   -> DirTree FileContent
--   -> CmdInput
-- inputDirTree name = inputDirNode name . DirNode . Dir

data CmdOutput = CmdOutput
  { outputCode :: !ExitCode
  , outputOut  :: !Sha256
  , outputErr  :: !Sha256
  } deriving (Show, Eq, Ord)

data CmdResult a = CmdResult
  { resultSetupTime :: !Double
  , resultRunTime   :: !Double
  , resultOutput    :: !a
  } deriving (Show, Eq, Ord)

resultExitCode :: CmdResult CmdOutput -> ExitCode
resultExitCode = outputCode . resultOutput

resultStdout :: CmdResult CmdOutput -> Sha256
resultStdout = outputOut . resultOutput

resultStderr :: CmdResult CmdOutput -> Sha256
resultStderr = outputErr . resultOutput

showHash :: BS.ByteString -> String
showHash =
  concatMap (printf "%02x") . BS.unpack

logResults ::
  (HasLogger env, MonadReader env m, MonadUnliftIO m)
  => Maybe CmdOutput
  -> m ()
logResults = \case
  Just (CmdOutput ec (showHash -> hos, olen) (showHash -> hes, elen)) -> do
    L.debug $ "exitcode:" <-> displayf "%3d" (exitCodeToInt ec)
    L.debug $ "stdout" <-> displayf "(bytes: %05d):" olen
      <-> Builder.fromString (take 8 hos)
    L.debug $ "stderr" <-> displayf "(bytes: %05d):" elen
      <-> Builder.fromString (take 8 hes)
  Nothing ->
    L.warn "Process Timed-out"

tryTimeout :: (MonadUnliftIO m, RealFrac r) => r -> m a -> m (Maybe a)
tryTimeout timelimit =
  if timelimit > 0
  then timeout (ceiling $ timelimit * 1e6)
  else fmap Just


exitCodeFromInt :: Int -> ExitCode
exitCodeFromInt = \case
  0 -> ExitSuccess
  n -> ExitFailure n

exitCodeToInt :: ExitCode -> Int
exitCodeToInt = \case
  ExitSuccess -> 0
  ExitFailure n -> n
