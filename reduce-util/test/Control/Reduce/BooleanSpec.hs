-- |
{-# LANGUAGE OverloadedStrings #-}

module Control.Reduce.BooleanSpec where

import Control.Reduce.Boolean
import Prelude hiding (not)

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
      let
        expr :: Nnf Int
        expr =
            (ff 4 \/ tt 0 /\ tt 1 )
            /\ (ff 4 \/ tt 24)
            /\ (ff 4 \/
                (tt 27
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
                ))
            /\ (ff 4 \/ tt 22)
            /\ (ff 4 \/ tt 28)
            /\ (ff 4 \/ tt 23)

      underDependencies expr `shouldBe`
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
      let
        expr :: Nnf Int
        expr =
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

      overDependencies expr `shouldBe`
        [ 4 ~~> 0
        , 4 ~~> 1
        , 4 ~~> 24
        , 4 ~~> 27
        , 4 ~~> 22
        , 4 ~~> 28
        , 4 ~~> 23
        ]


  describe "ReducedNnf" $ do
    it "can print this nice graph" $ do
      let x = dotReducedNnf (reduceNnf id (tt 1 ==> tt 2 /\ tt 2 ==> tt 3 /\ tt 1 ==> tt 3 :: Nnf Int))
      LazyText.putStrLn x
      x `shouldBe` ""





  --     length (cnfCompiler' expr) `shouldBe` 37

  -- describe "cnf compiler" $ do
  --   it "can compile a simple term" $ do
  --     cnfCompiler (
  --       TVar 1
  --       /\ TVar 2
  --       /\ ( neg (TVar 3) \/ TVar 4 )
  --       )
  --     `shouldBe`
  --       [ clause [(True, 1)], clause [(True, 2)], clause [(False, 3), (True, 4)]]

  --   it "can compile this monster" $ do
  --     cnfCompiler example1 `shouldBe`
  --       [clause [(True, 23), (False, 4)]]

  --   it "can compile this expression" $ do
  --     cnfCompiler (TVar 1 \/ TVar 2 \/ TVar 3 \/ TVar 4) `shouldBe`
  --       [clause [(True,1),(True,2),(True,3),(True,4)]]

  --   it "can compile this expression" $ do
  --     cnfCompiler (neg $ TVar 1 /\ TVar 2 /\ TVar 3 /\ TVar 4) `shouldBe`
  --       [clause [(False,1),(False,2),(False,3),(False,4)]]

  --   it "can compile this expression" $ do
  --     let expr = (forall [1, 2] TVar \/ forall [2,3] (neg . TVar))
  --     nnfFlatCompiler expr
  --       `shouldBe` tt 1 /\ tt 2 \/ ff 2 /\ ff 3

  --     cnfCompiler expr `shouldBe`
  --       [ clause [(True,1),(False,2)],clause [(True,1),(False,3)],clause [(True,2),(False,3)] ]

  --   it "can compile the bigest one" $ do
  --     let
  --       expr =
  --           (ff 4 \/
  --            tt 0 /\ tt 1 /\ tt 1 /\ tt 2 /\ tt 1 /\ tt 2 /\ tt 3 /\ tt 2 /\ tt 3
  --           )
  --           /\ (ff 4 \/ tt 24)
  --           /\ (ff 4 \/
  --               (tt 27
  --                \/ tt 26 /\ tt 10
  --                \/ tt 25 /\ (tt 10 /\ tt 9)
  --                \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 8)
  --               --  \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 14 /\ tt 11)
  --               --  \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 14 /\ tt 20 /\ tt 7)
  --               --  \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 12)
  --               --  \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 21 /\ tt 11)
  --               --  \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 21 /\ tt 20 /\ tt 7)
  --               --  \/ tt 24 /\ (tt 18 /\ tt 12)
  --               --  \/ tt 24 /\ (tt 18 /\ tt 21 /\ tt 11)
  --               --  \/ tt 24 /\ (tt 18 /\ tt 21 /\ tt 20 /\ tt 7)
  --               --  \/ tt 24 /\ (tt 19 /\ tt 13)
  --               --  \/ tt 24 /\ (tt 16 /\ tt 5)
  --               --  \/ tt 24 /\ (tt 17 /\ tt 6)
  --               -- ))
  --               ))
  --           /\ (ff 4 \/ tt 22)
  --           /\ (ff 4 \/ tt 28)
  --           /\ (ff 4 \/ tt 23)

  --     length (cnfCompiler' expr) `shouldBe` 37

  -- describe "nnf compiler" $ do
  --   it "can compile and flatten example1" $ do
  --     flattenNnf (nnfCompiler example1) `shouldBe`
  --       NOr (NVar False 4) (NVar True 23)

  --   it "can compile and flatten example2" $ do
  --     flattenNnf (nnfCompiler example2) `shouldBe`
  --       (ff 4 \/
  --        tt 0 /\ tt 1 /\ tt 1 /\ tt 2 /\ tt 1 /\ tt 2 /\ tt 3 /\ tt 2 /\ tt 3
  --       )
  --       /\ (ff 4 \/ tt 24)
  --       /\ (ff 4 \/
  --           (tt 27
  --            \/ tt 26 /\ tt 10
  --            \/ tt 25 /\ (tt 10 /\ tt 9)
  --            \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 8)
  --            \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 14 /\ tt 11)
  --            \/ tt 24 /\ (tt 10 /\ tt 9 /\ tt 14 /\ tt 20 /\ tt 7)
  --            \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 12)
  --            \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 21 /\ tt 11)
  --            \/ tt 24 /\ (tt 10 /\ tt 15 /\ tt 21 /\ tt 20 /\ tt 7)
  --            \/ tt 24 /\ (tt 18 /\ tt 12)
  --            \/ tt 24 /\ (tt 18 /\ tt 21 /\ tt 11)
  --            \/ tt 24 /\ (tt 18 /\ tt 21 /\ tt 20 /\ tt 7)
  --            \/ tt 24 /\ (tt 19 /\ tt 13)
  --            \/ tt 24 /\ (tt 16 /\ tt 5)
  --            \/ tt 24 /\ (tt 17 /\ tt 6)
  --           ))
  --       /\ (ff 4 \/ tt 22)
  --       /\ (ff 4 \/ tt 28)
  --       /\ (ff 4 \/ tt 23)

  -- --   it "can compile a more complex term" $ do
  -- --     cnfCompiler (neg (TVar (True, 1) /\ TVar (True, 2) /\ (TVar (False, 3) \/ TVar (True, 4))))
  -- --     `shouldBe`
  -- --       [ clause [(True, 1)], clause [(True, 2)], clause [(False, 3), (True, 4)]]

  
  -- -- return ()
