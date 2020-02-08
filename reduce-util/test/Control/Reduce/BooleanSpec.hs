{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Reduce.BooleanSpec where

-- base
import Data.Maybe
import Prelude hiding (not, and)
import qualified Data.List as List

-- lens
import Control.Lens

-- directory
import System.Directory

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- aeson
import Data.Aeson

-- containers
import qualified Data.IntSet as IS
import qualified Data.Set as S

-- vector
import qualified Data.Vector as V

-- text
import qualified Data.Text.Lazy.IO as LazyText

import Control.Reduce.Boolean
import Control.Reduce.Boolean.OBDD
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

example1 :: Nnf Int
example1 =
    (ff 4 \/ tt 0 /\ tt 1 )
    /\ (ff 4 \/ tt 24)
    /\ (tt 4
          ==> tt 27
          \/ (tt 26 /\ tt 10)
          \/ (tt 25 /\ (tt 10 /\ tt 9))
          \/ (tt 24 /\ (tt 10 /\ tt 9 /\ tt 8))
          \/ (tt 24 /\ (tt 10 /\ tt 9 /\ tt 14 /\ tt 11))
          \/ (tt 24 /\ (tt 10 /\ tt 9 /\ tt 14 /\ tt 20 /\ tt 7))
          \/ (tt 24 /\ (tt 10 /\ tt 15 /\ tt 12))
          \/ (tt 24 /\ (tt 10 /\ tt 15 /\ tt 21 /\ tt 11))
          \/ (tt 24 /\ (tt 10 /\ tt 15 /\ tt 21 /\ tt 20 /\ tt 7))
          \/ (tt 24 /\ (tt 18 /\ tt 12))
          \/ (tt 24 /\ (tt 18 /\ tt 21 /\ tt 11))
          \/ (tt 24 /\ (tt 18 /\ tt 21 /\ tt 20 /\ tt 7))
          \/ (tt 24 /\ (tt 19 /\ tt 13))
          \/ (tt 24 /\ (tt 16 /\ tt 5))
          \/ (tt 24 /\ (tt 17 /\ tt 6))
        )
    /\ (ff 4 \/ tt 22)
    /\ (ff 4 \/ tt 28)
    /\ (ff 4 \/ tt 23)

example2 :: Nnf Int
example2 =
    (ff 4 \/ tt 0 /\ tt 1 )
    /\ (ff 4 \/ tt 24)
    /\ (tt 4
          ==> tt 27
          \/ tt 26 /\ tt 10
          \/ tt 24 /\ (tt 17 /\ tt 6)
        )
    /\ (ff 4 \/ tt 22)

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


  describe "compressNnf" $ do
    it "can remove duplicates" $ do
      let x = compressNnf $ and
            [ tt 1 , tt 1
            , ff 2 \/ ff 2, ff 2
            , ff 4 \/ ff 3, ff 4 \/ ff 3 :: Nnf Int
            ]
          y = reduceNnf $ and
              [ tt 1, ff 3 \/ ff 4, ff 2 :: Nnf Int]
      showRnnf x `shouldBe` showRnnf y

    it "can reduce a clause" $ do
      print (IS.fromList [0])
      clauseLearning (S.fromList [ IS.fromList [ 0 ],  IS.fromList [ minBound, 1]])
        `shouldBe` (S.fromList [ IS.fromList [ 0 ], IS.fromList [ 1 ]])

    it "can do clause learning" $
      showRnnf ( compressNnf
        ( and [ tt 1 , ff 1 \/ tt 2, ff 2 \/ tt 3 :: Nnf Int]
        ) )
        `shouldBe`
        showRnnf ( reduceNnf (and [ tt 3, tt 2, tt 1 :: Nnf Int]))

  describe "extractNegation" $ do
    let ex4 = reduceNnf $ and [ tt 1 , ff 1 \/ tt 2 :: Nnf Int]
    it ("work on " ++ showRnnf ex4) do
      let x' = extractNegation 1 ex4
      let x = showRnnf x'
      x `shouldBe` "tt 2 ∧ tt 1 ∧ (tt 2 ∨ ff 1)"

    let ex1 = reduceNnf $ and [ tt 1 , ff 1 \/ tt 2, ff 2 \/ tt 3 :: Nnf Int]
    it ("work on " ++ showRnnf ex1) do
      let x' = extractNegation 1 ex1
      let x = showRnnf x'
      x `shouldBe` "tt 2 ∧ tt 1 ∧ (ff 2 ∨ tt 3) ∧ (tt 2 ∨ ff 1)"
      -- let y = showRnnf (extractNegation 2 x')
      -- y `shouldBe` "tt 1 ∧ (tt 2 ∧ tt 3)"

    let ex3 = reduceNnf $ and [ ff 1 :: Nnf Int]
    it ("work on " ++ showRnnf ex3) do
      let x' = extractNegation 1 ex3
      let x = showRnnf x'
      x `shouldBe` "ff 1"

    let ex2 = reduceNnf $ and [ ff 1 \/ ff 2 \/ tt 3:: Nnf Int]
    it ("work on " ++ showRnnf ex2) do
      let x' = extractNegation 1 ex2
      let x = showRnnf x'
      x `shouldBe` "ff 2 ∨ tt 3 ∨ ff 1"

    it "work on example 1" do
      let x' = extractNegation 4 (reduceNnf example1)
      show (compileObdd (nnfToTerm x'))
        `shouldBe`
        show (compileObdd (nnfToTerm example1))

    it "work on example 2" do
      let x' = extractNegation 4 (reduceNnf example2)
      -- let x = show x'
      compileObdd (nnfToTerm  x') `shouldBe`
        compileObdd (nnfToTerm  example2)

      compileObdd (nnfToTerm  $ extractNegation 2 (reduceNnf example2)) `shouldBe`
        compileObdd (nnfToTerm  example2)
      -- show x'
      --   `shouldBe`
      --   show (compileObdd (nnfToTerm example1))

    xit "large nnf" $ do
      x :: Maybe (Nnf Int) <- decode <$> BL.readFile "test/data/nnf.json"
      case x of
        Just t -> do
          let k = compressNnf t
          reduceNnfSize k `shouldBe` 11585

          k' <- go (List.sort . V.toList $ reduceNnfVars k) k
          reduceNnfSize k' `shouldBe` 11589

          where
            go [] term = return term
            go (n:rest) term = do
              let term' = extractNegation n term
              print (n, reduceNnfSize (term'))
              go rest term'

        Nothing ->
          expectationFailure "booo."

  describe "main-example" $ do

    let 
      varsOf :: (Foldable f, Ord a) => f (S.Set a, S.Set a) -> S.Set a
      varsOf = foldMap (\(a,b) -> S.union a b)

      condition :: Ord a => S.Set a 
        -> S.Set (S.Set a, S.Set a) 
        -> (S.Set a, S.Set (S.Set a, S.Set a))
      condition a =
        foldMap \case 
          (tts, ffs) 
            | a `S.disjoint` ffs ->
              let tts' = tts `S.difference` a
              in if S.null tts' && S.size ffs == 1 
              then (ffs, S.empty)
              else (S.empty, S.singleton (tts', ffs))
            | otherwise ->
              (S.empty, S.empty)

      propergate :: Ord a => S.Set a -> S.Set (S.Set a, S.Set a) 
        -> (S.Set a, S.Set (S.Set a, S.Set a))
      propergate a m = 
        let (a', m') = condition a m 
        in if S.null a'
        then (a' <> a, m')
        else propergate (a' <> a) m'
      
      splits :: Ord a => S.Set a -> S.Set (S.Set a, S.Set a) -> [(S.Set a, S.Set (S.Set a, S.Set a))]
      splits = go 
       where
        go vs cnf = case S.lookupMin vs of 
          Just p -> 
            let (items', cnf') = makeSat (S.singleton p) cnf
            in (items', cnf') : go (vs `S.difference` items') cnf'
          Nothing -> 
            []

        makeSat items cnf = 
          let (items', cnf') = propergate items cnf
          in case S.lookupMin cnf' of
            Just (trues, falses) 
             | S.null trues ->
               makeSat (S.insert (S.findMin falses) items') cnf'
            _ -> 
              (items', cnf')


    it "can read a big nnf" $ do 
      Just (nnf :: Nnf Int) <- decode <$> BL.readFile "test/data/nnf.json"
      let cnf = toFreshCNF nnf
      S.size cnf `shouldBe` 5090


  xdescribe "big nnf" $ do
    it "can read a nnf" $ do
      x :: Maybe (Nnf Int) <- decode <$> BL.readFile "test/data/nnf.json"
      case x of
        Just t -> do
          lengthOf folded t `shouldBe` 15868
          let k = compressNnf t
          reduceNnfSize k `shouldBe` 11585
          V.length (reduceNnfVars k) `shouldBe` 1871
          let (_, compressNnf -> k') = unify k
          reduceNnfSize k' `shouldBe` 9808
          V.length (reduceNnfVars k') `shouldBe` 1570
          k'' <- go 40 k'
          reduceNnfSize k'' `shouldBe` 25390

          -- 11738   11738
          -- 16713   15833
          -- 18631   26630
          -- 28225   46255
          -- 44476   48629
          -- 66856   82342
          -- 79096   76165
          -- 135454  120178
          -- 168570  113869
          -- 138534  168089
          -- 52915   308421
          -- 91162   377432
          -- 163880

          where
            go 0 term = return term
            go (n :: Int) term =
              case fst $ conflictingVar term of
                Just v -> do
                  let term' = splitRnnf v term
                  print (reduceNnfSize term')
                  go (n - 1) (compressNnf term')
                Nothing -> do
                  expectationFailure "expected more variables splits."
                  return term

        Nothing ->
          expectationFailure "booo."
