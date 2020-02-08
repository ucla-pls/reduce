module Control.Reduce.Boolean.CNFSpec where

import SpecHelper
import Prelude hiding (or, and)

-- import Data.Foldable

-- containers
import qualified Data.IntSet as IS

import Control.Reduce.Boolean
import Control.Reduce.Boolean.CNF 


spec :: Spec 
spec =
  fdescribe "basic functions" $ do
    it "can create a cnf" $ do
      let cnf = toCNF (ff 1 /\ ff 4 /\ tt 2 \/ tt 1 \/ tt 3 :: Nnf Int)
      cnfVariables cnf `shouldBe` IS.fromList [1, 2, 3, 4]
    it "can create a smaller cnf with fresh variables" $ do
      let ex = or
            [ tt 0 /\ tt 1 /\ tt 2
            , tt 3 /\ tt 4 /\ tt 5
            , tt 6 /\ tt 7 /\ tt 8 :: Nnf Int
            ]
      cnfSize (toMinimalCNF 100 ex) `shouldSatisfy` (< cnfSize (toCNF ex))

    it "can do forward propergation" $ do
      let ex = and [ ff 0 \/ tt 1
                   , ff 1 \/ tt 2 
                   , tt 4 \/ ff 1 \/ ff 2 :: Nnf Int 
                   ]
      let (a, b) = unitPropergation (unsafeFromLiterals [tt 0]) (toCNF ex) 
      debugCnf b
      a `shouldBe` unsafeFromLiterals [tt 0, tt 1, tt 2]



