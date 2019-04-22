{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Reduce.Language.CSpec where

import Data.ByteString as BS

-- text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- lens
import Control.Lens hiding (uses)

-- language-c
import           Language.C

-- neat-interpolation
import NeatInterpolation (text)

-- red
import Control.Reduce.Language.C
import Control.Reduce.Reduction

-- map
import qualified Data.Map.Strict as Map

-- base
import qualified Data.List.NonEmpty as NE

import SpecHelper


describeC :: String -> Text.Text -> SpecWith CTranslUnit -> Spec
describeC desc txt =
  before (return $ defineC "" txt) . describe desc

spec :: Spec
spec = do
  let hasItems l = it "has items" $ \ex ->
        shouldBe (toListOf (treeSubelements cR.withIndex._1.to NE.toList) (RCTranslUnit ex)) l
  let hasDefs l = it "has defs" $ \ex ->
        shouldMatchList (fmap (over _1 identToString) . Map.toList $ defs ex) l
  let hasUses l = it "has uses" $ \ex ->
        shouldMatchList (fmap (over _1 identToString) $ uses ex) l

  describeC "an empty file"
    [text|
         |] $ do
    hasItems []
    hasDefs []
    hasUses []


  describeC "a single function"
    [text| void f() {}
         |] $ do
    hasItems [[0]]
    hasDefs [("f", [[0]])]
    hasUses []


  describeC "functions with declarations and definitions"
    [text| void g();
           void g() {}
           void f() { g(); }
         |] $ do
    hasItems [[0], [1], [2], [0, 2]]
    hasDefs [("g", [[0],[1]]), ("f", [[2]])]
    hasUses [("g", [0,2])]


  describeC "structs"
    [text| struct List;
           struct List { int head; struct List * tail; } one;
           struct List two = { 1, &one };
         |] $ do
    hasItems [[0], [1], [2]]
    hasDefs
      [("List", [[0], [1]])
      , ("head", [[1]])
      , ("tail", [[1]])
      , ("one", [[1]])
      , ("two", [[2]])
      ]
    hasUses [("List", [1]), ("List", [2]), ("one", [2]) ]

  describeC "enums"
    [text| int a = 0;
           enum State;
           enum State {Working = a, Failed = 0} Maybe;
           enum State x = Working;
         |] $ do
    hasItems [[0], [1], [2], [3]]
    hasDefs
      [("a", [[0]])
      , ("State", [[1], [2]])
      , ("Working", [[2]])
      , ("Failed", [[2]])
      , ("Maybe", [[2]])
      , ("x", [[3]])
      ]
    hasUses [("a", [2]), ("Working", [3])]

  describeC "typedefs"
    [text| typedef int kmh;
           kmh speed = 0;
         |] $ do
     hasItems [ [0], [1] ]
     hasDefs [("kmh", [[0]]), ("speed", [[1]])]
     hasUses [("kmh", [1])]

defineC :: String -> Text.Text -> CTranslUnit
defineC str bs =
  case parseC (Text.encodeUtf8 bs) (initPos str) of
    Right x -> x
    Left m -> error $ show m
