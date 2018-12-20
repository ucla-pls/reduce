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
  toFormat . map toLower <$> strOption (short 'f' <> value "lines")

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
  , cnfFormat           :: !Format
  , cnfLogger           :: !Logger
  , cnfReducerName      :: !ReducerName
  , cnfPredicateOptions :: !PredicateOptions
  } deriving (Show)

getConfigParser :: Parser (IO Config)
getConfigParser =
  cfg
  <$> strOption (long "input-file" <> short 'i')
  <*> strOption (long "output-file" <> short 'o')
  <*> parseFormat
  <*> parseLogger
  <*> parseReducerName
  <*> parsePredicateOptions "red"
  where
    cfg input output fmt lg rn cmd =
      Config input output fmt lg rn <$> cmd

main :: IO ()
main = do
  config <- join . execParser $
    A.info (getConfigParser <**> helper)
    ( fullDesc
    <> header "red"
    <> progDesc "A command line tool for reducing almost anything."
    )
  runReaderT (run config) $ cnfLogger config

newtype Count = Count Int

instance C.DefaultOrdered Count where
  headerOrder _ = C.header ["count"]

instance C.ToNamedRecord Count where
  toNamedRecord (Count c) = C.namedRecord ["count" C..= c ]

run :: Config -> ReaderT Logger IO ()
run Config {..} = do
  case cnfFormat of
    Lines -> do
      bs <- liftIO $ BLC.readFile cnfInputFile
      result <- reduceAll
        (fromFile "input")
        Nothing
        (iso BLC.lines BLC.unlines) bs
      liftIO $ BLC.writeFile cnfOutputFile result
    Chars -> do
      bs <- liftIO $ BLC.readFile cnfInputFile
      result <- reduceAll
        (fromFile "input")
        Nothing
        (iso BLC.unpack BLC.pack) bs
      liftIO $ BLC.writeFile cnfOutputFile result
    Files -> do
      _ :/ dt <- liftIO $ fmap SameAs <$> readTree cnfInputFile
      result <- reduceAll
        (fromDirTree "input")
        Nothing
        (iso toFileList (fromJust . fromFileList)) dt
      liftIO $ writeTreeWith writeContent (cnfOutputFile :/ result)

  where
    reduceAll ::
      (HasLogger env, MonadReader env m, MonadUnliftIO m)
      => (a -> m CmdInput)
      -> Maybe ([b] -> Int)
      -> AnIso' a [b]
      -> a
      -> m a
    reduceAll tofile cost is a =
      toPredicateM
        cnfPredicateOptions tofile
        (Count . length . view (cloneIso is)) a >>= \case
        Just predicate -> do
          result <- reduce cnfReducerName cost predicate is a
          case result of
            Just r -> return r
            Nothing -> do
              L.warn "Could not reduce problem"
              return a
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
