{-# LANGUAGE OverloadedStrings #-}
module System.Process.ConsumeSpec where

import SpecHelper

import Crypto.Hash.SHA256 (hashlazyAndLength)
import Data.ByteString hiding (reverse)
import qualified Data.ByteString as BS

import System.Exit
import Data.IORef
import Control.Monad

import System.Process.Consume

foldTest :: Consumer m -> [ByteString] -> IO m
foldTest =
  uncurry foldM

spec :: Spec
spec = do
  describe "perLine" $ do
    it "should log lines" $ do
      logger <- perLine $ pureConsumer (flip (:)) []
      x <- foldTest logger ["Hello\nThis\nIs\nA\nString\n", ""]
      reverse x `shouldBe` ["Hello", "This", "Is", "A", "String",""]

    it "can be interupted" $ do
      logger <- perLine $ pureConsumer (flip (:)) []
      x <- foldTest logger
        [ "Hel", "lo", "\n", "This\nIs\n", "A\nString\n", "" ]
      reverse x `shouldBe` ["Hello", "This", "Is", "A", "String",""]

  describe "consume" $ do
    it "can run echo" $ do
      x <- consume
        (pureConsumer (BS.append) "")
        (pureConsumer (BS.append) "") "echo 'Hello, World!'"
      x `shouldBe`
        (ExitSuccess, "Hello, World!\n", "")

    -- it "can collect all lines" $ do
    --   ref <- newIORef []
    --   logger <- lineLogger $ \bs -> modifyIORef ref (++ [bs])
    --   void $ consume (defaultConsumerOptions { outLogger = logger }) "echo" ["Hello, World!\nThis\nIs\nA\nTest"]
    --   x <- readIORef ref
    --   x `shouldBe` ["Hello, World!", "This", "Is", "A", "Test", ""]

    -- it "can change directory" $ do
    --   dir <- consume (defaultConsumerOptions { workingFolder = Just "/" }) "pwd" []
    --   dir `shouldBe` (ExitSuccess, hashlazyAndLength "/\n", hashlazyAndLength "")
