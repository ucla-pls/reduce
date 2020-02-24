{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Control.Reduce.Boolean.CNFSpec where

import           SpecHelper

-- base 
import           Prelude                 hiding ( or
                                                , and
                                                , not
                                                )
import           Data.Maybe
import qualified Data.List.NonEmpty as NE
--import           Text.Show
--import           Data.Foldable hiding (or, and)
--import Data.Foldable hiding (or, and)

-- text
import qualified Data.Text                     as Text

-- aeson
import           Data.Aeson

-- vector
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC

-- containers
import qualified Data.IntSet                   as IS
import qualified Data.Set                   as S

import           Control.Reduce.Boolean
import           Control.Reduce.Boolean.CNF
import qualified Control.Reduce.Boolean.LiteralSet
                                               as LS


spec :: Spec
spec = do
  describe "basic functions" $ do
    it "can create a cnf" $ do
      let cnf = toCNF (ff 1 /\ ff 4 /\ tt 2 \/ tt 1 \/ tt 3 :: Nnf Int)
      cnfVariables cnf `shouldBe` IS.fromList [1, 2, 3, 4]

    it "can create a smaller cnf with fresh variables" $ do
      let ex = or
            [ tt 0 /\ tt 1 /\ tt 2
            , tt 3 /\ tt 4 /\ tt 5
            , tt 6 /\ tt 7 /\ tt 8 :: Nnf Int
            ]
      cnfSize (toMinimalCNF 99 ex) `shouldSatisfy` (< cnfSize (toCNF ex))

    it "can do forward propergation" $ do
      let ex     = and [ff 0 \/ tt 1, ff 1 \/ tt 2, ff 2 \/ tt 3 :: Nnf Int]
      let (a, _) = unitPropergation (LS.fromList' [tt 0]) (toCNF ex)
      a `shouldBe` Just (LS.fromList' [tt 0, tt 1, tt 2, tt 3])
  describe "vmap" $ do
    it "works on clauses" $ do
      LS.vmap
          (\case
            1 -> 0
            a -> a
          )
          (LS.fromList' [ff 0, tt 1] :: LS.Clause)
        `shouldBe` Nothing

  describe "possitive progression" $ do
    it "caluclate it on a small case" $ do
      let
        ex = toCNF
          $ and [ff 1 \/ tt 0, ff 2 \/ tt 3 \/ tt 1, ff 3 \/ tt 2 :: Nnf Int]
      
      progression 5 (V.fromList . S.toList . cnfClauses $ ex) 
        `shouldBe` (IS.fromList [0, 1, 2] NE.:| [IS.fromList [3], IS.fromList [4]])

    it "run it on a real case" $ do
      Just (ex :: Nnf Text.Text) <-
        fmap and . sequence . map decode . BLC.lines <$> BL.readFile
          "test/data/main-example.json"
      let (nnf, _) = memorizeNnf ex
      let cnf      = toMinimalCNF (maxVariable nnf) nnf
      weightedProgression 
        (fromIntegral . IS.size)
        (fromJust $ fromCNF cnf)
        `shouldBe` 
        ( IS.fromList [0,1,2,3,4,5,6,8,12,13,17,18,21,24]
        , [ IS.fromList [14,15]
          , IS.fromList [23]
          , IS.fromList [20]
          , IS.fromList [11]
          , IS.fromList [9,10]
          , IS.fromList [7]
          , IS.fromList [19]
          , IS.fromList [22]
          , IS.fromList [16]
          ]
        )

