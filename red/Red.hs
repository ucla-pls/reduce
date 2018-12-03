{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- The red application

-- mtl
import Control.Monad.Reader

-- text
import qualified Data.Text as Text

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC

-- optparse-applicative
import Options.Applicative


-- reduce-util
import Control.Reduce.Util
import System.Process.Consume

-- contravariant
import Data.Functor.Contravariant

-- reduce
import Control.Reduce
import Data.Functor.Contravariant.PredicateM

-- base
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import Data.Foldable

data Format = Format
  { formatFlag :: !Char
  , formatDescription :: !String
  } deriving (Show, Eq)

data Config = Config
  { format :: !Format
  , cnfLogger :: !SimpleLogger
  , checkOptions :: !CheckOptions
  , reducerOptions :: !ReducerOptions
  , cmdOptionsWithInput :: !CmdOptionWithInput
  } deriving (Show)

getConfigParser :: [ Format ] -> Parser (IO Config)
getConfigParser formats =
  cfg
  <$> asum (map formatAsOpt formats)
  <*> parseSimpleLogger
  <*> parseCheckOptions
  <*> parseReducerOptions
  <*> parseCmdOptionsWithInput
  where
    formatAsOpt fmt@(Format f desc) =
      flag' fmt (short f <> help desc)

    cfg fmt lg check rd cmd =
      Config fmt lg check <$> rd <*> cmd

main = do
  let configParser = getConfigParser formats
  config <- join . execParser $
    info (configParser <**> helper)
    ( fullDesc
    <> header "red"
    <> progDesc "A command line tool for reducing almost anything."
    )
  print config
  runReaderT (run config) $ cnfLogger config
  where
    formats =
      [ Format 'c' "see the input as a list of chars"
      , Format 'l' "see the input as a list of lines"
      ]

run :: Config -> ReaderT SimpleLogger IO ()
run Config {..} = do
  case cmdOptionsWithInput of
    StreamOptions a cmdOptions -> do
      mp <- toPredicateM checkOptions cmdOptions (workFolder $ reducerOptions) a
      case mp of
        Just pred ->
          case formatFlag format of
            'c' -> do
              x <- reduce reducerOptions "chars" (BLC.pack `contramap` pred) (BLC.unpack a)
              case x of
                Just f ->
                  liftIO $ BLC.putStrLn (BLC.pack f)
                Nothing ->
                  error "predicate was not reduceable"
            'l' -> do
              x <- reduce reducerOptions "lines" (BLC.unlines `contramap` pred) (BLC.lines a)
              case x of
                Just f ->
                  liftIO $ BLC.putStrLn (BLC.unlines f)
                Nothing ->
                  error "predicate was not reduceable"
        Nothing -> error "predicate was not valid"
    ArgumentOptions a cmdOptions -> do
      mp <- toPredicateM checkOptions cmdOptions (workFolder $ reducerOptions) a
      case mp of
        Just pred ->
          case formatFlag format of
            'c' -> do
              x <- reduce reducerOptions "chars" pred a
              case x of
                Just f ->
                  liftIO $ putStrLn f
                Nothing ->
                  error "predicate was not reduceable"
            'l' -> do
              x <- reduce reducerOptions "lines" (unlines `contramap` pred) (lines a)
              case x of
                Just f ->
                  liftIO $ putStrLn (unlines f)
                Nothing ->
                  error "predicate was not reduceable"
        Nothing -> error "predicate was not valid"
