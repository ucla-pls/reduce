{-# LANGUAGE OverloadedStrings #-}
module Control.Reduce.Util.CliPredicateSpec where

import           SpecHelper

import qualified Data.List                        as L
import qualified Data.Map                         as Map
import           UnliftIO

import           Control.Reduce.Util.CliPredicate
import           Data.Either

spec :: Spec
spec = do
  describe "parseCmdArgument" $ do
    it "can parse a constant string" $
      parseCmdArgument "--cp" `shouldBe` Right (CAConst "--cp")

    it "can parse a input command" $ do
      parseCmdArgument "{}" `shouldBe` Right (CAInput "")

    it "can parse a simple file" $ do
      parseCmdArgument "%text.sh" `shouldBe` Right (CAFilePath "text.sh")

    it "can parse a complex file" $ do
      parseCmdArgument "{%text.sh}" `shouldBe` Right (CAFilePath "text.sh")

    it "can parse a mix of different inputs" $ do
      parseCmdArgument "{}:{%file.txt}"
        `shouldBe`
        Right (CAJoin
               (CAInput "")
               (CAJoin
                (CAConst ":")
                (CAFilePath "file.txt")))

    it "can parse a constant with expanded style" $ do
      parseCmdArgument "{{}}" `shouldBe` Right (CAConst "{}")

  describe "canonicalizeOrFail" $ do
    it "should succeed if it can find the file" $ do
      canonicalizeOrFail "/dev/null"
      return ()

    it "should fail with an exception if it cannot find the file" $ do
      x <- tryIO $ canonicalizeOrFail "does not exist"
      x `shouldSatisfy` isLeft

      let Left y = x
      show y `shouldSatisfy` L.isSuffixOf "Expected file: does not exist"

  describe "createCmd" $ do
    it "finds executable and parses commandline arguments" $ do
      x <- createCmd 0 "echo" ["Hello, World"]
      x `shouldSatisfy` isRight

    it "returns left if it cannot find executable" $ do
      x <- createCmd 0 "not/an/executable" []
      x `shouldSatisfy` isLeft

    it "returns left if not all files exists" $ do
      x <- createCmd 0 "echo" ["%not-a-file.txt"]
      x `shouldSatisfy` isLeft

    it "returns left if it cannot parse the arguments" $ do
      x <- createCmd 0 "echo" ["{"]
      x `shouldSatisfy` isLeft

  describe "evaluateArgument" $ do
    it "looks up the input in a map" $ do
      evaluateArgument (CAInput "") (Map.singleton "" "hey")
        `shouldBe` "hey"
    it "default to the keyword when nothing is found" $ do
      evaluateArgument (CAInput "key") (Map.empty)
        `shouldBe` "{key}"
    it "can also handle concatenation" $ do
      let f = CAJoin (CAInput "") (CAJoin (CAConst ":") (CAFilePath "file.txt"))
      evaluateArgument f ((Map.singleton "" "key"))
        `shouldBe` "key:file.txt"

  describe "replaceRelative" $ do
    it "should replace a relative path" $ do
      replaceRelative "/hello/world/text.txt" ("/hello", "$VAR")
        `shouldBe` "$VAR/world/text.txt"

    it "should not replace an path outside the relative path" $ do
      replaceRelative "/hello/world/text.txt" ("/peanuts", "$VAR")
        `shouldBe` "/hello/world/text.txt"

  -- describe "createShellScript" $ do
  --   it "can create a shell script" $ do
  --     Right m <- createCmd "echo" ["Hello, World!"]
  --     createShellScript mempty "/workdir" m `shouldBe` ""
