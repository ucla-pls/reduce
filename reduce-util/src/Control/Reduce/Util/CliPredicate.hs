{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}

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
  Cmd (..)
  , createCmd
  , evaluateCmd

  , toShellScript

  , CmdArgument (..)
  , parseCmdArgument
  , evaluateArgument
  , argumentToString
  , canonicalizeArgument

  , relativeArgument
  , canonicalizeOrFail

  , CmdInput
  , runCmd

  , fromArgument
  , fromStream
  , fromDirTree
  , fromFile

  -- * Utils
  , replaceRelative
  , exitCodeFromInt
  , exitCodeToInt
  ) where


-- typed-process
import           System.Process.Typed

-- filepath
import           System.FilePath

-- text
import qualified Data.Text                             as Text
import qualified Data.Text.Lazy                        as LazyText
import qualified Data.Text.Lazy.Builder                as Builder
import qualified Data.Text.Lazy.Encoding               as Text
import qualified Data.Text.Lazy.IO                     as LazyText

-- unliftio
import           UnliftIO
import           UnliftIO.Directory

-- bytestring
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.ByteString.Lazy.Char8            as BLC

-- mtl
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer

-- lens
import           Control.Lens

-- containers
import qualified Data.Map                              as Map

-- base
import           Control.Applicative                   hiding (many, some)
import           Data.Bifunctor                        (first)
import           Data.Foldable
import           Data.Void
import           System.Exit
import           System.IO.Error
import           Text.Printf


-- process
import           System.Process                        (showCommandForUser)

-- megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- reduce-util
import           Control.Reduce.Util.Logger            as L
import           System.Process.Consume
import           System.Directory.Tree

-- reduce
import           Control.Reduce
import           Data.Functor.Contravariant.PredicateM

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
  first parseErrorPretty $ parse ((foldr1 (<>) <$> many cliArgumentP) <* eof) arg arg
  where
    cliArgumentP :: Parsec Void String CmdArgument
    cliArgumentP =
      msum
      [ CAFilePath <$> (char '%' *> some anyChar)
      , do
          x <- CAConst <$> (string "}}" *> return "}")
          return x
      , do
          _ <- char '{'
          msum
            [ CAConst . (:[]) <$> char '{'
            , CAFilePath <$> (char '%' *> some (notChar '}') <* char '}')
            , CAInput <$> many (notChar '}') <* char '}'
            ]
      , do
          CAConst <$> some (satisfy (\x -> x /= '{' && x /= '}'))
      ]

-- | Returns the arguments with all the file pointers
-- canonicalized. Throws an error if the file cannot be found.
-- See `canonicalizeOrFail`
canonicalizeArgument :: CmdArgument -> IO CmdArgument
canonicalizeArgument (CAJoin a b) =
  CAJoin
  <$> canonicalizeArugments a
  <*> canonicalizeArugments b
canonicalizeArugments (CAFilePath fp) = do
  CAFilePath <$> canonicalizeOrFail fp
canonicalizeArugments a =
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
relativeArgument (CAFilePath fp) = do
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
  when (not x)
    . ioError
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
    CAConst str -> const ca
    CAFilePath fp -> const ca
    CAInput key -> Map.findWithDefault ca key
    CAJoin a b ->
      CAJoin <$> evaluateArgument a <*> evaluateArgument b

argumentToString :: CmdArgument -> String
argumentToString = \case
  CAConst str -> str
  CAFilePath fp -> fp
  CAInput key -> "{" <> key <> "}"
  CAJoin a b ->
    argumentToString a <> argumentToString b

-- | A representation of a command-line command.
data Cmd = Cmd
  { cmdTimelimit :: !Double
  , cmdProgram   :: !FilePath
  , cmdArguments :: ![CmdArgument]
  } deriving (Show, Eq)

-- | Creates a new command from string arguments. Can fail if the executable or
-- any of the files mentioned in the arguments does not exits, or
-- if it can't parse the arguments.
createCmd :: Double -> String -> [String] -> IO (Either String Cmd)
createCmd timelimit cmd args = runExceptT $ do
  exactCmd <- tryL $ getExecutable cmd
  results <-
    mapM (tryL . canonicalizeArugments)
    =<< mapM (liftEither . parseCmdArgument) args
  return $ Cmd timelimit exactCmd results
  where
    getExecutable exec = do
      findExecutable exec >>=
        maybe (canonicalizeOrFail exec) return

    tryL :: IO a -> ExceptT String IO a
    tryL m = do
      x <- liftIO $ UnliftIO.tryIO m
      liftEither (first show x)

-- | Evalutates a `Cmd` with a keymap, so that the results can be
-- used to create a processs.
evaluateCmd ::
  Cmd
  -> (FilePath, String)
  -> Map.Map String CmdArgument
  -> (String, [String])
evaluateCmd (Cmd _ str args) fp kmap =
  ( replaceRelative str fp
  , map ( argumentToString
         . flip evaluateArgument kmap
         . flip relativeArgument fp) args
  )

data CmdInput = CmdInput
  { ciValueMap :: Map.Map String CmdArgument
  , ciStdin    :: Maybe BL.ByteString
  } deriving (Show, Eq)

instance Semigroup CmdInput where
  (<>) (CmdInput avm asi) (CmdInput bvm bsi) =
    CmdInput (avm <> bvm) (asi <> bsi)

instance Monoid CmdInput where
  mempty = CmdInput mempty mempty

fromArgument ::
  Monad m
  => String
  -> m CmdInput
fromArgument str =
  return $ mempty { ciValueMap = Map.singleton "" (CAConst str) }

fromStream ::
  Monad m
  => BL.ByteString
  -> m CmdInput
fromStream bs =
  return $ mempty { ciStdin = Just bs }

fromDirTree ::
  MonadIO m
  => String
  -> DirTree FileContent
  -> m CmdInput
fromDirTree name td = do
  liftIO $ writeTreeWith writeContent (name :/ td)
  return $ mempty { ciValueMap = Map.singleton "" (CAFilePath name)}

fromFile ::
  MonadIO m
  => String
  -> BL.ByteString
  -> m CmdInput
fromFile name bs = do
  liftIO $ BLC.writeFile name bs
  return $ mempty { ciValueMap = Map.singleton "" (CAFilePath name)}

-- | Creates a shell script which can be executed in the current working
-- directory.
toShellScript ::
  CmdInput
  -> FilePath
  -> Cmd
  -> LazyText.Text
toShellScript CmdInput {..} workdir cmd = do
  let
    (p, args) = evaluateCmd cmd (workdir, "\"$WORKDIR\"") ciValueMap
    bldr = execWriter $ do
      tell "#!/usr/bin/env sh\n"
      tell "WORKDIR=${1:-.}\n"
      tell "SANDBOX=${2:-\"$WORKDIR\"sandbox}\n"
      tell "mkdir \"$SANDBOX\"\n"
      tell "cd \"$SANDBOX\"\n"
      tell (Builder.fromString $ showCommandForUser p args)
      case ciStdin of
        Just _ ->
          tell "< \"$WORKDIR/stdin\""
        Nothing -> return ()
      tell "\n"
    in
    Builder.toLazyText bldr

-- | Creates a shell script which can be executed in the current working
-- directory.
setupWorkDir ::
  CmdInput
  -> Cmd
  -> IO ()
setupWorkDir cmdInput@(CmdInput {..}) cmd = do
  curDir <- getCurrentDirectory
  maybe (return ()) (BLC.writeFile "stdin") ciStdin
  LazyText.writeFile "run.sh" $ toShellScript cmdInput curDir cmd


-- | Run a command in a working folder. Fails if the working folder
-- already exists.
runCmd ::
 (HasLogger env, MonadReader env m, MonadUnliftIO m)
 => (a -> m CmdInput)
 -> FilePath
 -> Cmd
 -> a
 -> m (Maybe (ExitCode, Sha256, Sha256))
runCmd inputCreater workDir cmd a = do
  liftIO $ createDirectoryIfMissing True workDir
  withCurrentDirectory workDir $ do
    (tp, _) <- timedPhase "setup" $ do
      input <- inputCreater a
      liftIO $ setupWorkDir input cmd
    (tm, fmap formatResults -> m) <- timedPhase "run" $ do
      olog <- traceLogger "+"
      elog <- traceLogger "-"
      liftIO . withFile "stdout" WriteMode $
        \hout ->
          withFile "stderr" WriteMode $
          \herr ->
            tryTimeout (cmdTimelimit cmd) $
            consumeWithHash
              (combineConsumers olog $ handlerLogger hout)
              (combineConsumers elog $ handlerLogger herr)
              $ proc "run.sh" []
    logResults tp tm m
    return m

  where
    formatResults (ec, (_, ho), (_, he)) = (ec, ho, he)

    logResults tp tm m = do
      x <- case m of
        Just (ec, (showHash -> hos, olen), (showHash -> hes, elen)) -> do
          L.info $ "exitcode:" <-> displayf "%3d" (exitCodeToInt ec)
          L.info $ "stdout" <-> displayf "(bytes: %05d):" olen
            <-> Builder.fromString (take 8 hos)
          L.info $ "stderr" <-> displayf "(bytes: %05d):" elen
            <-> Builder.fromString (take 8 hes)
          return $ printf "%d,%s,%d,%s,%d\n" (exitCodeToInt ec)
        Nothing -> do
          L.warn $ "Process Timed-out"
          return $ "N/A,N/A,N/A,N/A,N/A\n"
      liftIO $ appendFile "process.csv" $
        printf "%.3f,%.3f," tp tm ++ x

    traceLogger ::
      (HasLogger env, MonadReader env m, MonadIO m)
      => Builder.Builder -> m (Consumer BS.ByteString ())
    traceLogger name = do
      env <- ask
      liftIO . perLine . logger $ maybe (return ()) (x env)
      where
        x env bs =
          runReaderT (L.log L.TRACE (name <-> bsToBuilder bs)) env
        bsToBuilder =
          Builder.fromLazyText . Text.decodeUtf8 . BLC.fromStrict

    showHash =
      concatMap (printf "%02x") . BS.unpack

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
