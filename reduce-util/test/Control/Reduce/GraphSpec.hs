{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Reduce.GraphSpec where

import           SpecHelper


import qualified Data.ByteString.Lazy as BL

import qualified Data.Text.Lazy.IO    as T

import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U

import qualified Data.IntSet          as IS
import qualified Data.Set             as S
import qualified Data.Tree            as T (Tree (Node))

import           Control.Reduce.Graph


spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "can build a singleton graph" $ do
      fst (buildGraph [('a', 'a', [('a', ())])]) `shouldBe`
        Graph (V.fromList [ buildNode 'a' [(0, ())]])

    it "can build a larger graph" $ do
      fst (buildGraph'
           [ ('a', "abc")
           , ('b', "")
           ]) `shouldBe`
        Graph (V.fromList
               [ buildNode 'a' [(0, ()), (1, ())]
               , buildNode 'b' []
               ]
              )
    -- it "can build an other graph" $ do
    --   let (gr, _) = buildGraph' [ ('a', "abc"), ('b', "bc"), ('c', "") ]
    --   gr `shouldBe` Control.Reduce.Graph.empty
  describe "scc, scc', and partition" $ do

    it "can handle a connected graph" $ do
      let (gr, _) = buildGraph' [ ('a', "abc"), ('b', "bc"), ('c', "a") ]
      scc' gr `shouldBe` [T.Node 0 [T.Node 1 [T.Node 2 []]]]
      scc gr `shouldBe` [IS.fromList [0, 1, 2]]
      componentMap gr `shouldBe` (V.fromList [IS.fromList [0, 1, 2]], U.fromList [0,0,0])
      partition gr `shouldBe` [(IS.fromList [0, 1, 2], IS.fromList [0, 1, 2])]

    it "can handle a sparse graph" $ do
      let (gr, _) = buildGraph' [ ('a', "abc"), ('b', "bc"), ('c', "") ]
      scc' gr `shouldBe` [T.Node 2 [], T.Node 1 [], T.Node 0 []]
      scc gr `shouldBe` [IS.fromList [2], IS.fromList [1], IS.fromList [0]]
      partition gr `shouldBe` [
        (IS.fromList [2], IS.fromList [2])
        , (IS.fromList [1], IS.fromList [1, 2])
        , (IS.fromList [0], IS.fromList [0, 1, 2])
        ]

  describe "readTGF" $ do
    it "can read a TGF formatted graph" $ do
      file <- T.readFile "test/data/small-graph.tgf"
      let r = readTGF "small-graph" file
      case r of
        Left t -> do
          putStrLn t
          fail "should be a right"
        Right x ->
          x `shouldBe` Graph (
          V.fromList
          [ buildNode "hello long label" [(1, "edge label")]
          , buildNode "world" []
          , buildNode "" [(0, "")]
          ]
          )

    it "can read a larger TGF formatted graph and extract the closures" $ do
      file <- T.readFile "test/data/example-graph.tgf"
      let r = readTGF "example-graph" file
      case r of
        Left t -> do
          putStrLn t
          fail "should be a right"
        Right x ->
          closuresN x `shouldMatchList`
          [ S.fromList [ "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"]
          , S.fromList [ "7", "8", "9", "10", "11", "12", "13", "14"]
          , S.fromList [ "1", "2", "3", "4", "7"]
          , S.fromList [ "1", "2", "3", "4", "5", "6", "7"]
          , S.fromList [ "1", "2", "4", "7"]
          , S.fromList [ "4", "7"]
          , S.fromList [ "7"]
          , S.fromList [ "0"]
          ]

  describe "readCSV" $ do
    it "can read a larger CSV formatted graph" $ do
      file <- BL.readFile "test/data/example-graph.csv"
      let r = readCSV [0..16 :: Int] file
      case r of
        Left t -> do
          putStrLn t
          fail "should be a right"
        Right ( x :: Graph () Int ) ->
          closuresN x `shouldMatchList`
          [ S.fromList [7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
          , S.fromList [7, 8, 9, 10, 11, 12, 13, 14]
          , S.fromList [1, 2, 3, 4, 7]
          , S.fromList [1, 2, 3, 4, 5, 6, 7]
          , S.fromList [1, 2, 4, 7]
          , S.fromList [4, 7]
          , S.fromList [7]
          , S.fromList [0]
          ]
