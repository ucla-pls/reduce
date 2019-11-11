-- |
{-# LANGUAGE OverloadedStrings #-}

module Control.Reduce.BooleanSpec where

import Control.Reduce.Boolean
import Prelude hiding (not, and)

import System.Directory

-- containers
import qualified Data.IntSet as IS

-- vector
import qualified Data.Vector as V

-- text
import qualified Data.Text.Lazy.IO as LazyText

import SpecHelper

-- example1 :: Term Int
-- example1 =
--   TAnd
--   (TAnd
--     (TConst True)
--     (TOr
--       (TNot (TVar 4))
--       (TAnd
--         (TAnd
--           (TOr
--             (TConst False)
--             (TAnd (TVar 23) (TConst True)))
--           (TAnd
--             (TConst True)
--             (TAnd
--               (TConst True)
--               (TConst True))
--           )
--         )
--         (TAnd
--          (TConst True)
--          (TAnd
--           (TConst True)
--           (TConst True)
--          )
--         )
--       )
--     )
--   )
--   (TOr (TNot (TVar 4)) (TConst True))

-- example2 :: Term Int
-- example2 = TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TConst True) (TOr (TNot (TVar 4)) (TConst True))) (TOr (TNot (TVar 4)) (TConst True))) (TOr (TNot (TVar 4)) (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TAnd (TConst True) (TVar 0)) (TVar 1)) (TVar 1)) (TVar 2)) (TVar 1)) (TVar 2)) (TVar 3)) (TVar 2)) (TVar 3)))) (TOr (TNot (TVar 4)) (TConst True))) (TOr (TNot (TVar 4)) (TAnd (TOr (TConst False) (TAnd (TVar 24) (TConst True))) (TAnd (TConst True) (TAnd (TConst True) (TConst True)))))) (TOr (TNot (TVar 4)) (TConst True))) (TOr (TNot (TVar 4)) (TConst True))) (TOr (TNot (TVar 4)) (TConst True))) (TOr (TNot (TVar 4)) (TAnd (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TOr (TConst False) (TAnd (TVar 27) (TConst True))) (TAnd (TVar 26) (TAnd (TConst True) (TVar 10)))) (TAnd (TVar 25) (TAnd (TAnd (TConst True) (TVar 10)) (TVar 9)))) (TAnd (TVar 24) (TAnd (TAnd (TAnd (TConst True) (TVar 10)) (TVar 9)) (TVar 8)))) (TAnd (TVar 24) (TAnd (TAnd (TAnd (TAnd (TConst True) (TVar 10)) (TVar 9)) (TVar 14)) (TVar 11)))) (TAnd (TVar 24) (TAnd (TAnd (TAnd (TAnd (TAnd (TConst True) (TVar 10)) (TVar 9)) (TVar 14)) (TVar 20)) (TVar 7)))) (TAnd (TVar 24) (TAnd (TAnd (TAnd (TConst True) (TVar 10)) (TVar 15)) (TVar 12)))) (TAnd (TVar 24) (TAnd (TAnd (TAnd (TAnd (TConst True) (TVar 10)) (TVar 15)) (TVar 21)) (TVar 11)))) (TAnd (TVar 24) (TAnd (TAnd (TAnd (TAnd (TAnd (TConst True) (TVar 10)) (TVar 15)) (TVar 21)) (TVar 20)) (TVar 7)))) (TAnd (TVar 24) (TAnd (TAnd (TConst True) (TVar 18)) (TVar 12)))) (TAnd (TVar 24) (TAnd (TAnd (TAnd (TConst True) (TVar 18)) (TVar 21)) (TVar 11)))) (TAnd (TVar 24) (TAnd (TAnd (TAnd (TAnd (TConst True) (TVar 18)) (TVar 21)) (TVar 20)) (TVar 7)))) (TAnd (TVar 24) (TAnd (TAnd (TConst True) (TVar 19)) (TVar 13)))) (TAnd (TVar 24) (TAnd (TAnd (TConst True) (TVar 16)) (TVar 5)))) (TAnd (TVar 24) (TAnd (TAnd (TConst True) (TVar 17)) (TVar 6)))) (TAnd (TConst True) (TAnd (TConst True) (TAnd (TConst True) (TConst True))))))) (TOr (TNot (TVar 4)) (TAnd (TAnd (TOr (TConst False) (TAnd (TVar 22) (TConst True))) (TAnd (TConst True) (TAnd (TConst True) (TConst True)))) (TAnd (TConst True) (TAnd (TConst True) (TConst True)))))) (TOr (TNot (TVar 4)) (TConst True))) (TOr (TNot (TVar 4)) (TConst True))) (TOr (TNot (TVar 4)) (TAnd (TOr (TConst False) (TAnd (TVar 28) (TConst True))) (TAnd (TConst True) (TAnd (TConst True) (TAnd (TConst True) (TConst True))))))) (TOr (TNot (TVar 4)) (TAnd (TAnd (TOr (TConst False) (TAnd (TVar 23) (TConst True))) (TAnd (TConst True) (TAnd (TConst True) (TConst True)))) (TAnd (TConst True) (TAnd (TConst True) (TConst True)))))) (TOr (TNot (TVar 4)) (TConst True))
example1 :: Nnf Int
example1 =
    (ff 4 \/ tt 0 /\ tt 1 )
    /\ (ff 4 \/ tt 24)
    /\ (tt 4
          ==> tt 27
          \/ tt 26 /\ tt 10
          \/ tt 25 /\ (tt 10 /\ tt 9)
          \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 8)
          \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 14 /\ tt 11)
          \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 14 /\ tt 20 /\ tt 7)
          \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 12)
          \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 21 /\ tt 11)
          \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 21 /\ tt 20 /\ tt 7)
          \/ tt 24 /\ (tt 18 /\ tt 12)
          \/ tt 24 /\ (tt 18 /\ tt 21 /\ tt 11)
          \/ tt 24 /\ (tt 18 /\ tt 21 /\ tt 20 /\ tt 7)
          \/ tt 24 /\ (tt 19 /\ tt 13)
          \/ tt 24 /\ (tt 16 /\ tt 5)
          \/ tt 24 /\ (tt 17 /\ tt 6)
        )
    /\ (ff 4 \/ tt 22)
    /\ (ff 4 \/ tt 28)
    /\ (ff 4 \/ tt 23)

spec :: Spec
spec = do
  describe "term" $ do
    it "should handle equality" $ do
      (tt 1 /\ tt 2 :: Term Int) `shouldBe`
       (tt 1 /\ tt 2)

    it "should print nicely" $ do
      show (tt 1 ∧ tt 2 ∨ ff 3 :: Term Int) `shouldBe`
       "tt 1 ∧ tt 2 ∨ not (tt 3)"

      show (tt 1 ∧ (tt 2 ∨ ff 3) :: Term Int) `shouldBe`
       "tt 1 ∧ (tt 2 ∨ not (tt 3))"

      show (tt 1 ∧ tt 2 ∧ ff 3 :: Term Int) `shouldBe`
       "tt 1 ∧ tt 2 ∧ not (tt 3)"

      show (tt 1 /\ tt 4 ==> tt 2 :: Term Int) `shouldBe`
       "not (tt 1 ∧ tt 4) ∨ tt 2"


  describe "nnf" $ do
    it "should handle equality" $ do
      (tt 1 ∧ tt 2 :: Nnf Int) `shouldBe`
       (tt 1 ∧ tt 2)

    it "should be able to not" $ do
      (not (tt 1 ∧ tt 2) :: Nnf Int) `shouldBe`
       (ff 1 ∨ ff 2)

    it "should enable compilation" $ do
      (crossCompiler $ neg (tt 1 ∧ tt 2 :: Term Int)) `shouldBe`
       (NnfAsTerm $ ff 1 \/ ff 2)

      (tt 1 /\ tt 4 ==> tt 2 :: Nnf Int) `shouldBe`
       (ff 1 ∨ ff 4 ∨ tt 2)
      

  describe "depenency" $ do
    it "should find a simple dependency" $ do
      underDependencies (tt 1 ∧ tt 2 :: Nnf Int) `shouldBe`
        [tt 1, tt 2]

    it "should find harder dependencies" $ do
      underDependencies (tt 1 ==> tt 2 :: Nnf Int) `shouldBe`
        [1 ~~> 2]

    it "should find even harder dependencies" $ do
      underDependencies (tt 1 ==> tt 2 /\ tt 3 /\ tt 4 /\ tt 5 :: Nnf Int) `shouldBe`
        [ 1 ~~> 2
        , 1 ~~> 3
        , 1 ~~> 4
        , 1 ~~> 5
        ]

    it "should underapproximate the logic" $ do
      underDependencies (tt 1 ==> tt 2 \/ tt 3 :: Nnf Int) `shouldBe`
        []

      underDependencies (tt 1 /\ tt 4 ==> tt 2 :: Nnf Int) `shouldBe`
        []

    it "should handle large expressions" $ do
      underDependencies example1 `shouldBe`
        [ 4 ~~> 0
        , 4 ~~> 1
        , 4 ~~> 24
        , 4 ~~> 22
        , 4 ~~> 28
        , 4 ~~> 23
        ]

    it "should overapprixmate the logic" $ do
      overDependencies (tt 1 ==> tt 2 \/ tt 3 :: Nnf Int) `shouldBe`
        [ 1 ~~> 2 ]

      overDependencies (tt 1 /\ tt 4 ==> tt 2 :: Nnf Int) `shouldBe`
        [ 1 ~~> 2 ]

    it "should handle big logics" $ do

      overDependencies example1 `shouldBe`
        [ 4 ~~> 0
        , 4 ~~> 1
        , 4 ~~> 24
        , 4 ~~> 27
        , 4 ~~> 22
        , 4 ~~> 28
        , 4 ~~> 23
        ]


  describe "reducedNnf" $ do

    it "can reduce: true" $ do
      reduceNnf (true :: Nnf Int) `shouldBe`
        ReducedNnf { redNnfHead = Left True, redNnfItems = V.empty }

    it "can reduce: false" $ do
      reduceNnf (false :: Nnf Int) `shouldBe`
        ReducedNnf { redNnfHead = Left False, redNnfItems = V.empty }

    it "can reduce: tt 1" $ do
      reduceNnf (tt 1 :: Nnf Int) `shouldBe`
        ReducedNnf
        { redNnfHead = Right 0
        , redNnfItems = V.fromList [ RLit 1 ]
        }

    it "can reduce: ff 1" $ do
      reduceNnf (ff 1 :: Nnf Int) `shouldBe`
        ReducedNnf
        { redNnfHead = Right minBound
        , redNnfItems = V.fromList [ RLit 1 ]
        }

    it "can reduce: ff 1 /\\ tt 1" $ do
      reduceNnf (ff 1 /\ tt 1 :: Nnf Int) `shouldBe`
        ReducedNnf
        { redNnfHead = Left False
        , redNnfItems = V.fromList []
        }

    it "can reduce: ff 1 \\/ tt 1" $ do
      reduceNnf (ff 1 \/ tt 1 :: Nnf Int) `shouldBe`
        ReducedNnf
        { redNnfHead = Left True
        , redNnfItems = V.fromList []
        }


  beforeAll (createDirectoryIfMissing True "test/outputs") $
    describe "run calculations" $ do
    -- it "can condition this expression" $ do
    --   let x =
    --         conditionNnf (ff 2) . conditionNnf (tt 1) . reduceNnf id $
    --           and [ tt 1 ==> tt 2
    --              , tt 2 ==> tt 3
    --              , tt 1 ==> tt 3 :: Nnf Int
    --              ]
    --   x `shouldBe` reduceNnf id (false :: Nnf Int)

    it "can calculate the minsat of the example1" $ do
      let x = reduceNnf example1
      LazyText.writeFile "test/outputs/example1-before.dot" (dotReducedNnf x)
      let y = compileDnnf x
      LazyText.writeFile "test/outputs/example1-after.dot" (dotReducedNnf y)
      minSat y `shouldBe` IS.fromList []

    it "can calculate the minsat another example" $ do
      let x = reduceNnf
            (and [ tt 1 \/ tt 4
                 , tt 2
                 , tt 3 /\ tt 5 \/ tt 4
                 ] :: Nnf Int
            )

      LazyText.writeFile "test/outputs/example.dot" (dotReducedNnf x)
      let y = compileDnnf x
      LazyText.writeFile "test/outputs/example-compiled.dot" (dotReducedNnf y)

      minSat y `shouldBe` IS.fromList [2, 4]

    it "can calculate the minsat of the example1 conditioned on 4" $ do
      let x = conditionNnf (tt 4) . reduceNnf $ example1
      LazyText.writeFile "test/outputs/example1-con-4-before.dot" (dotReducedNnf x)
      let y = compileDnnf x
      LazyText.writeFile "test/outputs/example1-con-4-after.dot" (dotReducedNnf y)
      minSat y `shouldBe` IS.fromList [0, 1, 22, 23, 24, 27, 28]

      ( conditionNnf (tt 1)
       . conditionNnf (tt 0)
       . conditionNnf (tt 22)
       . conditionNnf (tt 23)
       . conditionNnf (tt 24)
       . conditionNnf (tt 27)
       . conditionNnf (tt 28)
        $ x
        ) `shouldBe` reduceNnf (true :: Nnf Int)
