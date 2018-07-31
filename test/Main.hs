import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec.Expectations.Pretty
import Test.Tasty.Hspec hiding (shouldBe)

import           Control.Monad.Trans.Maybe

import Control.Monad.Identity

import Control.Reduce
import qualified Data.List as L

main = defaultMain =<< tests

tests :: IO TestTree
tests =
  testGroup "reduce" <$> sequence
    [ testSpec "binarySearch" specBinarySearch
    , testGroup "base" <$> sequence
      [ testSpec "ddmin" (baseTests ddmin)
      , testSpec "binaryReduction" (baseTests binaryReduction)
      , testSpec "revBinaryReduction" (baseTests revBinaryReduction)
      ]
    ]

baseTests :: (forall m. Monad m => Reducer [Int] m) -> Spec
baseTests red = do
  it "returns Nothing, if the predicate is false for all inputs" $ do
    x <- red (\i -> return False) [0..10]
    x `shouldBe` Nothing
  it "returns Just [], if the predicate is true for the empty list" $ do
    x <- red (\i -> return True) [0..10]
    x `shouldBe` Just []
  it "can find a single element" $ do
    x <- red (\i -> return (L.elem 5 i)) [0..10]
    x `shouldBe` Just [5]
  it "can find two elements" $ do
    x <- red (\i -> return ([3, 5] `L.isSubsequenceOf` i)) [0..10]
    x `shouldBe` Just [3, 5]
  it "can find three elements" $ do
    x <- red (\i -> return ([3, 5, 9] `L.isSubsequenceOf` i)) [0..10]
    x `shouldBe` Just [3, 5, 9]

specBinarySearch :: Spec
specBinarySearch = do
  it "can find 5 in the range [0, 10]" $ do
    i <- binarySearch (\i -> guard (i >= 5)) 0 10
    i `shouldBe` 5
  it "can't find 11 in the range [0, 10]" $ do
    i <- runMaybeT $ binarySearch (\i -> guard (i >= 11)) 0 10
    i `shouldBe` Nothing
  it "returns the smallest element if all is true" $ do
    i <- binarySearch (\_ -> guard True) 0 10
    i `shouldBe` 0
