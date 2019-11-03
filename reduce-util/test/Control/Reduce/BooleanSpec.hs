-- |

module Control.Reduce.BooleanSpec where

import Control.Reduce.Boolean

import SpecHelper

spec :: Spec
spec = do
  describe "cnf compiler" $ do
    it "can compile a simple term" $ do
      cnfCompiler (
        TVar (True, 1)
        /\ TVar (True, 2)
        /\ ( TVar (False, 3)
             \/ TVar (True, 4)
           )
        )
      `shouldBe`
        [ clause [(True, 1)], clause [(True, 2)], clause [(False, 3), (True, 4)]]

  --   it "can compile a more complex term" $ do
  --     cnfCompiler (neg (TVar (True, 1) /\ TVar (True, 2) /\ (TVar (False, 3) \/ TVar (True, 4))))
  --     `shouldBe`
  --       [ clause [(True, 1)], clause [(True, 2)], clause [(False, 3), (True, 4)]]

  
  -- return ()
