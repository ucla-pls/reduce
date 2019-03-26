{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Reduce.ReductionSpec where

import           SpecHelper

import           Control.Lens hiding ((.=))

import           Control.Reduce.Reduction

import           Data.Aeson


spec :: Spec
spec = do
  describe "list" $ do
    it "should iterate through a list" $ do
      ("Hey" ^.. foldR listR ) `shouldBe` "Hey"

    it "should be able to count the number or reducable elements in " $ do
      (countR listR "Hey") `shouldBe` 3

    it "should keep second element in a list " $ do
      (limitR (== (1 :: Int)) (indexingR listR) "Hey") `shouldBe` Just "e"

  describe "json" $ do
    it "shold be countable" $ do
      (countR jsonR (object [ "name" .= True, "other" .= False ]))
        `shouldBe` 2

    it "shold be countable" $ do
      (limitR (== (1 :: Int)) (indexingR jsonR) (toJSON [ True, False ]))
        `shouldBe` Just (toJSON [ False ])

    it "should contain a deep tree of reduction" $ do
      let v :: Value = toJSON [[True], [False, True]]
      (itoListOf (ifoldR (deepR jsonR)) v)
        `shouldBe` ([ ([], toJSON [[True], [False, True]])
                    , ([0], toJSON [True])
                    , ([0,0], Bool True)
                    , ([1], toJSON [False, True])
                    , ([1,0], Bool False)
                    , ([1,1], Bool True)
                    ] :: [([Int], Value)])
