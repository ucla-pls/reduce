{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-|
Module      : Control.Reduce.Util.Logger
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module contains a logger implementation.
-}

module Control.Reduce.Util.Logger where

-- lens
import           Control.Lens

-- text
import qualified Data.Text.Lazy         as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO      as Text

-- unliftio
import           UnliftIO               (MonadUnliftIO (..))

-- mtl
import           Control.Monad.Reader

-- time
import           Data.Time              (diffUTCTime, getCurrentTime,
                                         getZonedTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime,
                                         iso8601DateFormat)

-- base
import           Prelude                hiding (log)
import           System.IO
import           Text.Printf

data LogLevel
  = TRACE
  | DEBUG
  | INFO
  | WARN
  | ERROR
  deriving (Show, Eq, Ord, Enum, Bounded)

data IndentionFormat = IndentionFormat
  { straight :: !Text.Text
  , new      :: !Text.Text
  , end      :: !Text.Text
  } deriving (Show, Eq)

data LoggerConfig = LoggerConfig
  { logLevel     :: ! LogLevel
  , currentDepth :: ! Int
  , maxDepth     :: ! Int
  , logHandle    :: ! Handle
  , indent       :: ! IndentionFormat
  , silent       :: ! Bool
  } deriving (Show, Eq)

class HasLogger env where
  loggerL :: Lens env env LoggerConfig LoggerConfig

instance HasLogger LoggerConfig where
  loggerL = id

newtype LoggerT m a =
  LoggerT { runLoggerT :: ReaderT LoggerConfig m a }
  deriving (Functor, Applicative, Monad, MonadReader LoggerConfig, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (LoggerT m) where
  withRunInIO inner = LoggerT $ withRunInIO $ \run -> inner (run . runLoggerT)

type Logger = LoggerT IO

withLogger :: (MonadReader env m, HasLogger env, MonadIO m) => LoggerT IO a -> m a
withLogger m =
  liftIO . runReaderT (runLoggerT m) =<< view loggerL

runLogger :: LoggerConfig -> Logger a -> IO a
runLogger lc m =
  liftIO $ runReaderT (runLoggerT m) lc

defaultLogger :: LoggerConfig
defaultLogger = LoggerConfig INFO 0 0 stderr (IndentionFormat "│ " "├ " "└ ") False

silentLogger :: LoggerConfig
silentLogger = defaultLogger { silent = True }

withLoggerConfig :: Monad m => (LoggerConfig -> m a) -> LoggerT m a
withLoggerConfig = LoggerT . ReaderT
{-# INLINE withLoggerConfig #-}

sPutStr :: MonadIO m => LoggerConfig -> m Builder.Builder -> m ()
sPutStr LoggerConfig {..} m =
  unless silent $
  liftIO . Text.hPutStr logHandle . Builder.toLazyText =<< m

sPutStrLn :: MonadIO m => LoggerConfig -> m Builder.Builder -> m ()
sPutStrLn LoggerConfig {..} m =
  unless silent $
  liftIO . Text.hPutStrLn logHandle . Builder.toLazyText =<< m

simpleLogMessage ::
  MonadIO m
  => LoggerConfig
  -> String
  -> Builder.Builder
  -> m (Builder.Builder)
simpleLogMessage LoggerConfig {..} lvl bldr = do
  t <- liftIO $ getZonedTime
  return $
    (
      if logLevel <= DEBUG
      then
        displayf "[%5s]" lvl
        <-> Builder.fromString
        (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) t)
      else
        if logLevel == INFO
        then
          Builder.fromString ((formatTime defaultTimeLocale "%H:%M:%S") t)
        else
          mempty
    ) <-> indentation currentDepth (straight indent) <> bldr
  where
    indentation i cur =
      Builder.fromLazyText (Text.replicate (fromIntegral i) cur)

log ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => LogLevel -> Builder.Builder -> m ()
log curLvl bldr = do
  sl@LoggerConfig {..} <- view loggerL
  when (curLvl >= logLevel && (currentDepth <= maxDepth || maxDepth < 0)) $ do
      sPutStrLn sl $ simpleLogMessage sl (show curLvl)
        (Builder.fromLazyText (new indent) <> bldr)

logtime ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => LogLevel -> Builder.Builder -> m a -> m a
logtime curLvl bldr ma = do
  sl@LoggerConfig {..} <- view loggerL
  if (curLvl >= logLevel && (currentDepth <= maxDepth || maxDepth < 0))
    then do
      sPutStr sl $ simpleLogMessage sl (show curLvl)
        (Builder.fromLazyText (new indent) <> bldr)
      (t, a) <- timeIO ma
      sPutStrLn sl . return $ displayf " (%.3fs)" t
      return a
    else do
      ma

timedPhase' ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder -> m (Double, a) -> m (Double, a)
timedPhase' bldr ma = do
  sl@LoggerConfig {..} <- view loggerL
  let runMa = local (over loggerL $ \s -> s { currentDepth = currentDepth + 1 } ) ma
  case currentDepth `compare` (if maxDepth < 0 then currentDepth + 1 else maxDepth) of
    LT -> do
      sPutStrLn sl $ simpleLogMessage sl "START"
          (Builder.fromLazyText (new indent) <> bldr)
      (t, a) <- runMa
      sPutStrLn sl . simpleLogMessage sl "END" $
        ( Builder.fromLazyText (straight indent)
          <> Builder.fromLazyText (end indent)
          <> displayf "%.3fs" t )
      return (t, a)
    EQ -> do
      sPutStr sl . simpleLogMessage sl "PHASE" $
          Builder.fromLazyText (new indent) <> bldr
      (t, a) <- runMa
      sPutStrLn sl . return $ displayf " (%.3fs)" t
      return (t, a)
    GT ->
      runMa

timedPhase ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder -> m a -> m (Double, a)
timedPhase bldr = timedPhase' bldr . timeIO
{-# INLINE timedPhase #-}

phase ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder -> m a -> m a
phase bldr ma = snd <$> timedPhase bldr ma
{-# INLINE phase #-}

-- * Helpers

trace ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m ()
trace = log TRACE

debug ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m ()
debug = log DEBUG

info ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m ()
info = log INFO

warn ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m ()
warn = log WARN

err ::
  (HasLogger env, MonadReader env m, MonadIO m)
  => Builder.Builder
  -> m ()
err = log ERROR


-- * Builder helpers

(<->) :: Builder.Builder -> Builder.Builder -> Builder.Builder
(<->) a b = a <> " " <> b

display :: Show a => a -> Builder.Builder
display = Builder.fromString . show

displayf :: PrintfArg a => String -> a -> Builder.Builder
displayf fmt = Builder.fromString . printf fmt

displayString :: String -> Builder.Builder
displayString = Builder.fromString

displayText :: Text.Text -> Builder.Builder
displayText = Builder.fromLazyText

timeIO :: MonadIO m => m a -> m (Double, a)
timeIO m = do
  start <- liftIO getCurrentTime
  a <- m
  end <- liftIO getCurrentTime
  return (realToFrac $ diffUTCTime end start, a)
