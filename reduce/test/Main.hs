{-# LANGUAGE RankNTypes #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec.Expectations.Pretty
import Test.Tasty.Hspec hiding (shouldBe, shouldSatisfy, shouldMatchList)

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer

import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Control.Reduce
import qualified Data.List as L
import qualified Data.IntSet as IS

main = defaultMain =<< tests

tests :: IO TestTree
tests =
  testGroup "reduce" <$> sequence
    [ testSpec "binarySearch" specBinarySearch
    , testGroup "base" <$> sequence
      [ testSpec "ddmin" (baseTests ddmin)
      , testSpec "binaryReduction" (baseTests binaryReduction)
      , testSpec "linearReduction" (baseTests linearReduction)
      , testSpec "setBinaryReduction"
          (baseTests (liftISetReducer setBinaryReduction ))
      , testSpec "genericBinaryReduction"
          (baseTests (genericBinaryReduction (const 1)))
      ]
    , testGroup "sets" <$> sequence
        [ testSpec "gBiRed" $ setsTests (toSetReducer $ genericBinaryReduction (IS.size . IS.unions))
        , testSpec "sBiRed" $ setsTests setBinaryReduction
        ]
    ]

baseTests :: (forall m. Monad m => Reducer m [Int]) -> Spec
baseTests red = do
  it "returns Nothing, if the predicate is false for all inputs" $ do
    x <- red (yes . const False) [0..10]
    x `shouldBe` Nothing
  it "returns Just [], if the predicate is true for the empty list" $ do
    x <- red (yes . const True) [0..10]
    x `shouldBe` Just []
  it "can find a single element" $ do
    x <- red (yes . L.elem 5) [0..10]
    x `shouldBe` Just [5]
  it "can find two elements" $ do
    x <- red (yes . L.isSubsequenceOf [3, 5]) [0..10]
    x `shouldBe` Just [3, 5]
  it "can find three elements" $ do
    x <- red (yes . L.isSubsequenceOf [3, 5, 9]) [0..10]
    x `shouldBe` Just [3, 5, 9]
  it "can find a minimum " $ do
    let minima = [[3, 5, 9], [5,8,9], [1,2]]
    Just x <- red ((\i -> yes $ any (flip L.isSubsequenceOf i) minima)) [0..10]
    x `shouldSatisfy` (flip L.elem) minima

setsTests :: (forall m. Monad m => ISetReducer m) -> Spec
setsTests red = do
  it "extreme test" $ do
    let
      big = IS.fromList [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,22,23,24,25,27,28,29,39,40,41,42,43,44,65,86,87,88,89,90,91,92,93,94,95,97,98,102,103,104,105,106,107,108]
      space =
          [IS.fromList [62]
          ,IS.fromList [30]
          ,IS.fromList [19]
          ,IS.fromList [104]
          ,IS.fromList [65]
          ,IS.fromList [108]
          ,IS.fromList [102]
          ,IS.fromList [103]
          ,IS.fromList [105]
          ,IS.fromList [43]
          ,IS.fromList [3]
          ,IS.fromList [2]
          ,IS.fromList [69,103]
          ,IS.fromList [2,27]
          ,IS.fromList [106,108]
          ,IS.fromList [92,103]
          ,IS.fromList [86,87]
          ,IS.fromList [42,43]
          ,IS.fromList [41,43]
          ,IS.fromList [3,94]
          ,IS.fromList [2,23]
          ,IS.fromList [3,90]
          ,IS.fromList [2,29]
          ,IS.fromList [2,3,64]
          ,IS.fromList [30,33,103]
          ,IS.fromList [106,107,108]
          ,IS.fromList [97,98,102]
          ,IS.fromList [39,40,105]
          ,IS.fromList [2,23,93]
          ,IS.fromList [2,29,89]
          ,IS.fromList [99,100,101,102]
          ,IS.fromList [91,92,102,103]
          ,IS.fromList [2,23,27,93,96]
          ,IS.fromList [2,23,27,93,95]
          ,IS.fromList [39,40,43,44,105]
          ,IS.fromList [39,40,41,42,43,44,45,105]
          ,IS.fromList [86,87,88,91,92,97,98,102,103]
          ,IS.fromList [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,22,23,24,25,27,28,29,39,40,41,42,43,44,65,86,87,88,89,90,91,92,93,94,95,97,98,102,103,104,105,106,107,108]
          ,IS.fromList [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,22,23,24,25,27,28,29,39,40,41,42,43,44,63,65,86,87,88,89,90,91,92,93,94,95,97,98,102,103,104,105,106,107,108]
          ]
    x <- red (yes . IS.isSubsetOf big) space
    x `shouldBe` Just [big]

specBinarySearch :: Spec
specBinarySearch = do
  it "can find 5 in the range [0, 10]" $ do
    i <- binarySearch (\i -> guard (i >= 5)) 0 10
    i `shouldBe` 5
  it "is not a linear search" $ do
    (i, x) <- runWriterT $ binarySearch (\i -> tell [i] >> liftIO (guard (i >= 5))) 0 10
    x `shouldBe` [5]
  it "can't find 11 in the range [0, 10]" $ do
    i <- runMaybeT $ binarySearch (\i -> guard (i >= 11)) 0 10
    i `shouldBe` Nothing
  it "returns the smallest element if all is true" $ do
    i <- binarySearch (\_ -> guard True) 0 10
    i `shouldBe` 0

yes :: Monad m => (Bool -> m Bool)
yes = return
