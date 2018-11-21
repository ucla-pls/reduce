{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-|
Module      : System.Process.Consume
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module reveals a process consumer. A process consumer is
like a process but is able to consume the results while running
the program.

* TODO

- Handle Ctrl^C
-}
module System.Process.Consume where

-- async
import           Control.Concurrent.Async

-- typed-process
import           System.Process.Typed

-- filepath
import           System.FilePath

-- directory
import           System.Directory

-- bytestring
import qualified Data.ByteString.Lazy     as BL
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8    as BS

-- cryptohash-sha256
import           Crypto.Hash.SHA256       as Sha256

-- stm
import           Control.Concurrent.STM

-- mtl
import           Control.Monad.Writer
import           Control.Monad.Reader.Class

-- base
import           Control.Monad
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.Word
import           System.Exit
import           System.IO
import           System.IO.Error


type SHA256 = (BS.ByteString, Word64)

-- | Consumes the output of a command and condenses it into a sha256
consume ::
  Consumer BS.ByteString stdout
  -> Consumer BS.ByteString stderr
  -> ProcessConfig a b c
  -> IO (ExitCode, stdout, stderr)
consume outLogger errLogger cfg = do
  withProcess (setStderr createPipe . setStdout createPipe $ cfg) $ \p -> do
    out <- async ( hFoldM 256 (getStdout p) outLogger )
    err <- async ( hFoldM 256 (getStderr p) errLogger )
    atomically $
      (,,) <$> waitExitCodeSTM p <*> waitSTM out <*> waitSTM err
{-# inline consume #-}

-- * Consumer

-- | A consumer, is a fold over a handle.
type Consumer b a = (a -> b -> IO a, a)

-- | A logger is a consumer with no side effects
type Logger b = Consumer b ()

-- | Fold over a handle using a consumer
hFoldM :: Int -> Handle -> Consumer BS.ByteString a -> IO a
hFoldM size handle consumer = go (snd consumer)
  where
    go !acc = do
      str <- BS.hGetSome handle size
      acc' <- fst consumer acc str
      if BS.null str
        then return $! acc'
        else go acc'
{-# inline hFoldM #-}

-- | Turn a logger of lines into a LogFunc
perLine :: Consumer (Maybe BS.ByteString) a -> IO (Consumer BS.ByteString a)
perLine (consumer, i) = do
  ref <- newIORef BS.empty
  return (go ref, i)
  where
    go ref a bs
      | BS.null bs = do
          left <- readIORef ref
          if BS.null left
            then
            consumer a (Nothing )
            else do
            consumer a (Just left)
            consumer a Nothing
      | otherwise = do
          left <- readIORef ref
          let cont:rest = BS.split '\n' bs
          (a', left') <-
            foldM (\(a, bs') bs -> (,bs) <$> consumer i (Just bs'))
                  (a, left `BS.append` cont) rest
          writeIORef ref left'
          return a'

-- | Build a pure consumer. A pure consumer does not use the IO monad.
pureConsumer :: (a -> b -> a) -> a -> Consumer b a
pureConsumer fn a = ((\a' bs -> return $ fn a' bs), a)
{-# inline pureConsumer #-}

-- | Ignores the arguments
ignoreConsumer :: Consumer b ()
ignoreConsumer = logger (const $ return ())

combineConsumers :: Consumer c a -> Consumer c b -> Consumer c (a, b)
combineConsumers (fna, ia) (fnb, ib) =
  ((\(a, b) bs -> (,) <$> fna a bs <*> fnb b bs), (ia, ib))


-- ** Loggers

-- | Build a logger
logger :: (b -> IO ()) -> Logger b
logger fn = ((\() -> fn), ())
{-# inline logger #-}

perLineLogger :: (Maybe BS.ByteString -> IO ()) -> IO (Logger BS.ByteString)
perLineLogger = perLine . logger

class HasLoggers env where
  stdoutLog :: env -> Logger BS.ByteString
  stderrLog :: env -> Logger BS.ByteString

logProcess :: (HasLoggers env, MonadReader env m, MonadIO m) => ProcessConfig a b c -> m ExitCode
logProcess pc = do
  olog <- asks stdoutLog
  elog <- asks stderrLog

  (ec,_, _) <- liftIO $ consume olog elog pc
  return ec

-- ** HashConsumer

type Sha256 = (BS.ByteString, Word64)

hashConsumer :: Consumer BS.ByteString (Sha256.Ctx)
hashConsumer = (\ctx bs -> return $ Sha256.update ctx bs, Sha256.init)

getHash :: Sha256.Ctx -> Sha256
getHash = Sha256.finalizeAndLength

consumeWithHash ::
  Consumer BS.ByteString stdout
  -> Consumer BS.ByteString stderr
  -> ProcessConfig a b c
  -> IO (ExitCode, (stdout, Sha256), (stderr, Sha256))
consumeWithHash stdout stderr =
  fmap (\(e, (out, octx), (err, ectx)) ->
          (e, (out, getHash octx), (err, getHash ectx)))
  . consume
    (combineConsumers stdout hashConsumer)
    (combineConsumers stderr hashConsumer)

logWithHash ::
 (HasLoggers env, MonadReader env m, MonadIO m)
  => ProcessConfig a b c
  -> m (ExitCode, Sha256, Sha256)
logWithHash pc = do
  olog <- asks stdoutLog
  elog <- asks stderrLog

  (ec,(_,ho),(_,he)) <- liftIO $ consumeWithHash olog elog pc
  return (ec, ho, he)
