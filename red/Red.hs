{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- mtl
import           Control.Monad.Reader

-- text
import qualified Data.Text.Lazy.Builder as Builder

-- bytestring
import qualified Data.ByteString.Lazy.Char8   as BLC

-- optparse-applicative
import           Options.Applicative          as A

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger   as L
import           Control.Reduce.Util.OptParse

-- contravariant
import           Data.Functor.Contravariant

-- base
import           Data.Foldable
import           System.Exit

data Format = Format
  { formatFlag        :: !Char
  , formatDescription :: !String
  } deriving (Show, Eq)

data Config = Config
  { format              :: !Format
  , cnfLogger           :: !SimpleLogger
  , checkOptions        :: !CheckOptions
  , reducerOptions      :: !ReducerOptions
  , cmdOptionsWithInput :: !CmdOptionWithInput
  } deriving (Show)

getConfigParser :: [ Format ] -> Parser (IO Config)
getConfigParser formats =
  cfg
  <$> asum (map formatAsOpt formats)
  <*> parseSimpleLogger
  <*> parseCheckOptions
  <*> parseReducerOptions "red"
  <*> parseCmdOptionsWithInput
  where
    formatAsOpt fmt@(Format f desc) =
      flag' fmt (short f <> help desc)

    cfg fmt lg check rd cmd =
      Config fmt lg check <$> rd <*> cmd

main :: IO ()
main = do
  let configParser = getConfigParser formats
  config <- join . execParser $
    A.info (configParser <**> helper)
    ( fullDesc
    <> header "red"
    <> progDesc "A command line tool for reducing almost anything."
    )
  runReaderT (run config) $ cnfLogger config
  where
    formats =
      [ Format 'c' "see the input as a list of chars"
      , Format 'l' "see the input as a list of lines"
      ]

type Iso a b = (a -> b, b -> a)

go ::
  ReducerOptions
  -> CheckOptions
  -> CmdOptions a
  -> String
  -> Iso a [b]
  -> a
  -> ReaderT SimpleLogger IO (Maybe a)
go redOpt checkOpt cmdOpt name (from, to) a = do
  let folder = workFolder redOpt
  mp <- toPredicateM checkOpt cmdOpt folder a
  case mp of
    Just pred' -> do
      x <- reduce redOpt name (to `contramap` pred') (from a)
      return $ fmap to x
    Nothing -> do
      L.err "Predicate failed"
      liftIO $ exitWith (ExitFailure 1)
{-# INLINE go #-}

run :: Config -> ReaderT SimpleLogger IO ()
run Config {..} = do
  case cmdOptionsWithInput of
    StreamOptions a cmdOptions -> do
      let red = go reducerOptions checkOptions cmdOptions
      mp <- case formatFlag format of
          'c' -> red "chars" (BLC.unpack, BLC.pack) a
          'l' -> red "lines" (BLC.lines, BLC.unlines) a
          c ->
            logAndExit $ "does not handle" <-> display c
      case mp of
        Just res ->
          liftIO $ BLC.putStrLn res
        Nothing ->
          logAndExit $ "input was not reducable"

    ArgumentOptions a cmdOptions -> do
      let red = go reducerOptions checkOptions cmdOptions
      mp <- case formatFlag format of
          'c' -> red "chars" (id, id) a
          'l' -> red "lines" (lines, unlines) a
          c ->
            logAndExit $ "does not handle" <-> display c
      case mp of
        Just res ->
          liftIO $ putStrLn res
        Nothing ->
          error "input was not reducable"

logAndExit ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m b
logAndExit bldr = do
  L.err bldr
  liftIO $ exitWith (ExitFailure 1)
