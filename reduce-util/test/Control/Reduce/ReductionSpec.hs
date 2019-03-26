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
      ("Hey" ^.. subelements listR ) `shouldBe` "Hey"

    it "should be able to count the number or reducable elements in " $ do
      (lengthOf (subelements listR) "Hey") `shouldBe` 3

    it "should keep second element in a list " $ do
      (limiting listR (==1) "Hey") `shouldBe` "e"

  describe "json" $ do
    it "shold be countable" $ do
      (lengthOf (subelements jsonR) (object [ "name" .= True, "other" .= False ]))
        `shouldBe` 2

    it "shold be countable" $ do
      (limiting jsonR (==1) (toJSON [ True, False ]))
        `shouldBe` toJSON [ False ]

    let v :: Value = toJSON [[True], [False, True]]

    it "should contain a deep tree of reduction" $ do
      itoListOf (deepsubelements (adventure jsonR)) v
        `shouldBe` ([ ([], toJSON [[True], [False, True]])
                    , ([0], toJSON [True])
                    , ([0,0], Bool True)
                    , ([1], toJSON [False, True])
                    , ([1,0], Bool False)
                    , ([1,1], Bool True)
                    ] :: [([Int], Value)])

    it "should contain a deep tree of reduction" $ do
      limit (deepening (adventure jsonR))
        (\i -> i == [1, 0] || i == [1] || i == []) v
        `shouldBe` Just (toJSON [[False]])

    it "should remove everything if the root is not includeded" $ do
      limit (deepening (adventure jsonR))
        (\i -> i == [1, 0] || i == [1]) v
        `shouldBe` Nothing
