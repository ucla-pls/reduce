{-# LANGUAGE OverloadedStrings #-}
module Control.Reduce.UtilSpec where

import SpecHelper

import Crypto.Hash.SHA256 (hashlazyAndLength)
import Data.ByteString

import System.Exit
import Data.IORef
import Control.Monad

import Control.Reduce.Util

spec :: Spec
spec = do
  describe "lineLogger" $ do
    it "should log lines" $ do
      ref <- newIORef []
      logger <- lineLogger $ \bs -> modifyIORef ref (++ [bs])
      logger "Hello\nThis\nIs\nA\nString\n"
      logger ""
      x <- readIORef ref
      x `shouldBe` ["Hello", "This", "Is", "A", "String",""]
    it "can be interupted" $ do
      ref <- newIORef []
      logger <- lineLogger $ \bs -> modifyIORef ref (++ [bs])
      logger "Hel"
      logger "lo"
      logger "\n"
      logger "This\nIs\n"
      logger "A\nString\n"
      logger ""
      x <- readIORef ref
      x `shouldBe` ["Hello", "This", "Is", "A", "String",""]

  describe "consume" $ do
    it "can run echo" $ do
      x <- consume defaultConsumerOptions "echo" ["Hello, World!"]
      x `shouldBe` (ExitSuccess, hashlazyAndLength "Hello, World!\n", hashlazyAndLength "")

    it "can collect all lines" $ do
      ref <- newIORef []
      logger <- lineLogger $ \bs -> modifyIORef ref (++ [bs])
      void $ consume (defaultConsumerOptions { outLogger = logger }) "echo" ["Hello, World!\nThis\nIs\nA\nTest"]
      x <- readIORef ref
      x `shouldBe` ["Hello, World!", "This", "Is", "A", "Test", ""]

    it "can change directory" $ do
      dir <- consume (defaultConsumerOptions { workingFolder = Just "/" }) "pwd" []
      dir `shouldBe` (ExitSuccess, hashlazyAndLength "/\n", hashlazyAndLength "")

  describe "fromCommand" $ do
    it "should find that true returns ExitSuccess" $ do
      b <- fromCommand defaultConsumerOptions [Status ExitSuccess] "true" [] "hello"
      b `shouldBe` True
    it "should find that false does not return ExitSuccess" $ do
      b <- fromCommand defaultConsumerOptions [Status ExitSuccess] "false" [] "hello"
      b `shouldBe` False
    it "should find that false does not return ExitFailure" $ do
      b <- fromCommand defaultConsumerOptions [Status (ExitFailure 1)] "false" [] "hello"
      b `shouldBe` True
    it "should find that echo does both print the results and exit with success" $ do
      b <- fromCommand defaultConsumerOptions [Status ExitSuccess, StdOutHash (hashlazyAndLength "hello\n")] "echo" [] "hello"
      b `shouldBe` True
