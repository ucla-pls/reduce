{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Red where

-- mtl
import           Control.Monad.Reader

-- unliftio
import UnliftIO

-- lens
import Control.Lens.Iso

-- text
import qualified Data.Text.Lazy.Builder           as Builder

-- bytestring
import qualified Data.ByteString.Lazy.Char8       as BLC

-- optparse-applicative
import           Options.Applicative              as A

-- reduce
import           Data.Functor.Contravariant.PredicateM

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.CliPredicate
import           Control.Reduce.Util.Logger       as L
import           Control.Reduce.Util.OptParse
import           System.Directory.Tree

-- cassava
import qualified Data.Csv as C

-- contravariant
import           Data.Functor.Contravariant

-- base
import           Data.Foldable
import           Data.Maybe
import           Data.Char
import qualified Data.List as List
import           System.Exit


data Format
  = Chars
  | Lines
  | Files
  deriving (Show, Read, Ord, Eq)

parseFormat :: Parser Format
parseFormat =
  toFormat . map toLower
  <$> strOption
  ( short 'f' <> value "lines"
    <> showDefault <> help "the format of the input."
    <> metavar "FORMAT"
  )
  where
    toFormat str =
      let f = List.isPrefixOf str in
      if | f "chars" -> Chars
         | f "lines" -> Lines
         | f "files" -> Files
         | True ->
           error $ "Unknown format " ++ str



data Config = Config
  { cnfInputFile        :: !FilePath
  , cnfOutputFile       :: !FilePath
  , cnfLogger           :: !Logger
  , cnfReducerName      :: !ReducerName
  , cnfFormat           :: !Format
  , cnfPredicateOptions :: !PredicateOptions
  } deriving (Show)

getConfigParser :: Parser (IO Config)
getConfigParser =
  cfg
  <$> strOption (long "input-file" <> short 'i')
  <*> strOption (long "output-file" <> short 'o')
  <*> parseLogger
  <*> parseReducerName
  <*> parseFormat
  <*> parsePredicateOptions "red"
  where
    cfg input output lg rn fmt cmd =
      Config input output lg rn fmt <$> cmd

main :: IO ()
main = do
  config <- join . execParser $
    A.info (getConfigParser <**> helper)
    ( fullDesc
    <> header "red"
    <> progDesc "A command line tool for reducing almost anything."
    )
  runReaderT (run config) $ cnfLogger config

run :: Config -> ReaderT Logger IO ()
run Config {..} = do
  case cnfFormat of
    Lines -> do
      bs <- liftIO $ BLC.readFile cnfInputFile
      result <- reduceAll
        (fromFile "input")
        Nothing BLC.unlines
        $ BLC.lines bs
      liftIO $ BLC.writeFile cnfOutputFile result
    Chars -> do
      bs <- liftIO $ BLC.readFile cnfInputFile
      result <- reduceAll
        (fromFile "input")
        Nothing BLC.pack $ BLC.unpack bs
      liftIO $ BLC.writeFile cnfOutputFile result
    Files -> do
      _ :/ dt <- liftIO $ fmap SameAs <$> readTree cnfInputFile
      result <- reduceAll
        (fromDirTree "input")
        Nothing (fromJust . fromFileList)
        $ toFileList dt
      liftIO $ writeTreeWith writeContent (cnfOutputFile :/ result)

  where
    reduceAll ::
      (HasLogger env, MonadReader env m, MonadUnliftIO m)
      => (a -> m CmdInput)
      -> Maybe ([b] -> Int)
      -> ([b] -> a)
      -> [b]
      -> m a
    reduceAll tofile cost f bs =
      toPredicateM cnfPredicateOptions (tofile . unCount) (f <$> counted bs)
      >>= \case
        Just predicate -> do
          result <- reduce cnfReducerName cost predicate (fmap f . counted) bs
          case result of
            Just r -> return (unCount r)
            Nothing -> do
              L.warn "Could not reduce problem"
              return (f $ bs)
        Nothing -> do
          L.err "Predicate failed"
          liftIO $ exitWith (ExitFailure 1)


logAndExit ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m b
logAndExit bldr = do
  L.err bldr
  liftIO $ exitWith (ExitFailure 1)
