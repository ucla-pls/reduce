{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module      : Control.Reduce.Util.Logger
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module contains a logger implementation.
-}

module Control.Reduce.Util.Logger where

-- text
import qualified Data.Text.Lazy             as Text
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.IO          as Text

-- mtl
import           Control.Monad.Reader.Class

-- time
import           Data.Time                  (diffUTCTime, getCurrentTime,
                                             getZonedTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             iso8601DateFormat)

-- base
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor.Const
import           Data.Functor.Identity
import           Prelude                    hiding (log)
import           System.IO
import           Text.Printf

data LogLevel
  = DEBUG
  | INFO
  | WARN
  | ERROR
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Has logger is a class, It allows two functions: `phase` and `log`.
-- `log` logs information, and `phase` logs a phase of information.
-- Logger is nested in the IO monad.
class HasLogger env where
  log ::
    (MonadReader env m, MonadIO m)
    => LogLevel -> Builder.Builder -> m ()
  timedPhase' ::
    (MonadReader env m, MonadIO m)
    => Builder.Builder -> m (Double, a) -> m (Double, a)

data IndentionFormat = IndentionFormat
  { straight :: !Text.Text
  , new      :: !Text.Text
  , end      :: !Text.Text
  } deriving (Show, Eq)

data SimpleLogger = SimpleLogger
  { logLevel     :: ! LogLevel
  , currentDepth :: ! Int
  , maxDepth     :: ! Int
  , logHandle    :: ! Handle
  , indent       :: ! IndentionFormat
  } deriving (Show, Eq)

defaultLogger :: SimpleLogger
defaultLogger = SimpleLogger INFO 0 0 stderr (IndentionFormat "│ " "├ " "└ ")

sPutStr :: MonadIO m => SimpleLogger -> m Builder.Builder -> m ()
sPutStr SimpleLogger {..} m =
  liftIO . Text.hPutStr logHandle . Builder.toLazyText =<< m

sPutStrLn :: MonadIO m => SimpleLogger -> m Builder.Builder -> m ()
sPutStrLn SimpleLogger {..} m =
  liftIO . Text.hPutStrLn logHandle . Builder.toLazyText =<< m

simpleLogMessage ::
  MonadIO m
  => SimpleLogger
  -> String
  -> Builder.Builder
  -> m (Builder.Builder)
simpleLogMessage SimpleLogger {..} lvl bldr = do
  t <- liftIO $ getZonedTime
  return $
    case logLevel of
      DEBUG ->
        displayf "[%5s]" lvl
        <-> Builder.fromString
          (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) t)
      INFO ->
          Builder.fromString ((formatTime defaultTimeLocale "%H:%M:%S") t)
      _ ->
          mempty
    <-> indentation currentDepth (straight indent) <> bldr

  where
    indentation i cur =
      Builder.fromLazyText (Text.replicate (fromIntegral i) cur)

instance HasLogger SimpleLogger where
  log curLvl bldr = do
    sl@SimpleLogger {..} <- ask
    when (curLvl >= logLevel && (currentDepth <= maxDepth || maxDepth < 0)) $ do
       sPutStrLn sl $ simpleLogMessage sl (show curLvl)
         (Builder.fromLazyText (new indent) <> bldr)

  timedPhase' bldr ma = do
    sl@SimpleLogger {..} <- ask
    let runMa = local (\s -> s { currentDepth = currentDepth + 1 } ) ma
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

timeIO :: MonadIO m => m a -> m (Double, a)
timeIO m = do
  start <- liftIO getCurrentTime
  a <- m
  end <- liftIO getCurrentTime
  return (realToFrac $ diffUTCTime end start, a)

-- Lens boilerplate
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

view :: MonadReader s m => Lens s s a a -> m a
view lens = getConst . lens Const <$> ask

update :: Lens s t a b -> (a -> b) -> (s -> t)
update lens f = runIdentity . lens (Identity . f)
