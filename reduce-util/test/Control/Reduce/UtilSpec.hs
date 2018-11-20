{-# LANGUAGE OverloadedStrings #-}
module Control.Reduce.UtilSpec where

import SpecHelper

import System.Exit
import Crypto.Hash.SHA256

import Control.Reduce.Util
import System.Directory

spec :: Spec
spec = do
  describe "mkCliPredicate" $ do
    it "should find that true returns ExitSuccess" $ do
      pred <- mkCliPredicate
        . setTest [Status ExitSuccess]
        <$> mkCliOptions "true"
      b <- runPredicateM pred "hello"
      b `shouldBe` True

    it "should find that false does not return ExitSuccess" $ do
      pred <- mkCliPredicate
        . setTest [Status ExitSuccess]
        <$> mkCliOptions "false"
      b <- runPredicateM pred "hello"
      b `shouldBe` False

    it "should find that false does not return ExitFailure" $ do
      pred <- mkCliPredicate
        . setTest [Status (ExitFailure 1)]
        <$> mkCliOptions "false"
      b <- runPredicateM pred "hello"
      b `shouldBe` True

    it "should find that echo both print the results and exit with success" $ do
      pred <- mkCliPredicate
        . setTest
        [ Status ExitSuccess
        , StdOutHash (hashlazyAndLength "hello\n")
        ]
        <$> mkCliOptions "echo"
      b <- runPredicateM pred "hello"
      b `shouldBe` True
