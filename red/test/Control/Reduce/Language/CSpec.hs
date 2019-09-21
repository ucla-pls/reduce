{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Reduce.Language.CSpec where

-- base
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor

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
import Control.Reduce.Graph

-- containers
import qualified Data.Map.Strict as Map

import SpecHelper


describeC :: String -> Text.Text -> SpecWith CTranslUnit -> Spec
describeC desc txt =
  before (return $ defineC "" txt) . describe desc

-- fdescribeC :: String -> Text.Text -> SpecWith CTranslUnit -> Spec
-- fdescribeC desc txt =
--   before (return $ defineC "" txt) . fdescribe desc

spec :: Spec
spec = do
  let hasItems l = it "has items" $ \ex ->
        shouldBe (toListOf (treeSubelements cR.withIndex._1.to NE.toList) (RCTranslUnit ex)) l
  let hasDefs l = it "has defs" $ \ex ->
        shouldMatchList (fmap (over _1 identToString) . Map.toList $ defs ex) l
  let hasUses l = it "has uses" $ \ex ->
        shouldMatchList (fmap (over _1 identToString) $ uses ex) l
  let hasDefUses l = it "has def-uses" $ \ex ->
        shouldMatchList (fmap (first identToString) $ defuses ex) l

  describeC "an empty file"
    [text|
         |] $ do
    hasItems []
    hasDefs []
    hasUses []
    hasDefUses []


  describeC "a single function"
    [text| void f() {}
         |] $ do
    hasItems [[0]]
    hasDefs [("f", [[0]])]
    hasUses []
    hasDefUses []


  describeC "functions with declarations and definitions"
    [text| void g();
           void g() {}
           void f() { g(); }
         |] $ do
    hasItems [[0], [1], [2], [0, 2]]
    hasDefs [("g", [[0],[1]]), ("f", [[2]])]
    hasUses [("g", [0,2])]
    hasDefUses [Edge "g" [0,2] [1] ]


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
    hasDefUses
      [ Edge "one"  [2] [1]
      , Edge "List" [2] [1]
      , Edge "List" [1] [0]
      ]

  describeC "structs 2 "
    [text| struct Const { int item; };
           void f(struct Const a) {
             struct Const a1;
             }
         |] $ do
    hasItems [[0], [1], [0, 1]]
    hasDefs
      [("Const", [[0]])
      , ("item", [[0]])
      , ("a", [[1]])
      , ("a1", [[0, 1]])
      , ("f", [[1]])
      ]
    hasUses [("Const", [1]), ("Const", [0, 1])]
    hasDefUses [Edge "Const" [1] [0] , Edge "Const" [0,1] [0] ]

  describeC "structs 3"
    [text| struct Const { int item;} c;
           struct Const x;
         |] $ do
    hasItems [[0], [1]]
    hasDefs
      [("Const", [[0]])
      , ("item", [[0]])
      , ("c", [[0]])
      , ("x", [[1]])
      ]
    hasUses [("Const", [1])]
    hasDefUses [Edge "Const" [1] [0] ]


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
    hasUses [("a", [2]), ("Working", [3]), ("State", [3])]
    hasDefUses
      [ Edge "a"       [2] [0]
      , Edge "State"   [3] [2]
      , Edge "Working" [3] [2]
      ]

  describeC "function"
    [text| typedef int kmh;
           void f(kmh x);
         |] $ do
        hasItems [ [0], [1]]
        hasDefs [("kmh", [[0]]), ("f", [[1]])]
        hasUses [("kmh", [1])]

  describeC "typedefs"
    [text| typedef struct x kmh;
           kmh speed = 0;
           void f(kmh x);
         |] $ do
    hasItems [ [0], [1], [2]]
    hasDefs [("kmh", [[0]]), ("speed", [[1]]), ("f", [[2]])]
    hasUses [("kmh", [1]), ("kmh", [2]), ("x", [0]) ]
    hasDefUses
      [ Edge "kmh" [1] [0]
      , Edge "kmh" [2] [0]
      ]

defineC :: String -> Text.Text -> CTranslUnit
defineC str bs =
  case parseC (Text.encodeUtf8 bs) (initPos str) of
    Right x -> x
    Left m -> error $ show m


newtype NeverEq a = NeverEq a
  deriving (Show)

instance Eq (NeverEq a) where
  _ == _ = False
