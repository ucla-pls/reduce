{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Reduce.Boolean.OBDDSpec where

-- prelude
import System.Exit
-- import Data.Foldable
-- import Control.Monad

import Control.Lens
import qualified Data.Vector as V

-- aesone
import Data.Aeson (decode)

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- directory
import System.Directory

-- typed-process
import System.Process.Typed

import qualified Data.Text.Lazy.IO as LazyText


import Control.Reduce.Boolean.OBDD
import Control.Reduce.Boolean
import Control.Reduce.BooleanSpec
import SpecHelper

spec :: Spec
spec = do
  describe "compileObdd" $ do
    it "can compile to false" $ do
      compileObdd (tt 1 /\ ff 1)
        `shouldBe` false

    it "can compile to tt 2" $ do
      compileObdd (tt 1 /\ ff 1 \/ tt 2)
        `shouldBe` compileObdd (tt 2)

    it "can compile to example1" $ do
      obddTop (compileObdd (toStmt (NnfAsStmt example1)))
        `shouldBe` 423


  beforeAll (createDirectoryIfMissing True "test/outputs") $
    describe "dotObdd" $ do
    it "can display simple example" $ do
      LazyText.writeFile "test/outputs/obdd.dot"
        (dotObdd (compileObdd (tt 1 /\ tt 2 \/ tt 3 /\ ff 2)))
      runProcess "dot -Tpdf test/outputs/obdd.dot -o test/outputs/obdd.pdf"
        `shouldReturn` ExitSuccess

    it "can display example1" $ do
      LazyText.writeFile "test/outputs/obdd-example1.dot"
        (dotObdd (compileObdd (toStmt (NnfAsStmt example1))))
      runProcess "dot -Tpdf test/outputs/obdd-example1.dot -o test/outputs/obdd-example1.pdf"
        `shouldReturn` ExitSuccess
  
  -- fdescribe "cudd" $ do
  --   it "can do some stuff" $ do
  --     Just x :: Maybe (Nnf Int) <- decode <$> BL.readFile "test/data/nnf.json"
  --     -- termToCudd (toStmt (NnfAsStmt x))

  xdescribe "big nnf" $ do
    it "can process the big nnf" $ do
      x :: Maybe (Nnf Int) <- decode <$> BL.readFile "test/data/nnf.json"
      case x of
        Nothing -> expectationFailure "booo."
        Just t -> do
          let (_, k') = unify (compressNnf t)
          let cnj = asConjunction (compressNnf k')
          let v = V.map (compileObdd . toStmt . NnfAsStmt) (V.fromList cnj)
          V.length v `shouldBe` 3047
          v' <- compressV v
          v'' <- compressV v'
          _ <- compressV v''
          -- _ <- compressV v'''
          -- v''' <- compressV v''
          -- v'''' <- compressV v'''
          -- _ <- compressV v''''
          return ()

        where
          compressV :: V.Vector Obdd -> IO (V.Vector Obdd)
          compressV v = do
            let
              (x, y) = V.splitAt (V.length v `div` 2) v
              z = V.accumulate (/\) y (V.imap (,) x)
            iforM_ z \i z' ->
              print (i, obddTop z')
            return z
 

