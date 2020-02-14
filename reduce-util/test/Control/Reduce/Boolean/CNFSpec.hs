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

    -- it "can map some thing" $ do 
    --   let ex = and [ ff 0 \/ tt 1
    --                , ff 1 \/ tt 2 
    --                , ff 2 \/ tt 3 :: Nnf Int 
    --                ]
    --   -- debugCnf (vmapCNF (\case 1 -> 0; a -> a) (toCNF ex))

  describe "toMinimalCnf" $ do
    fit "can handle an example" $ do
      let
        ex :: Nnf String
        ex =
          ( not
                (tt
                  "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                )
            ∨ tt "laundrycommon/commonclass/accordion/AccordionRootItem"
            )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ tt "laundrycommon/commonclass/accordion/AccordionMenu"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionMenu"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionRootItem"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionRootItem"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionRootItem"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionBranch"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionBranch"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionMenu"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionMenu"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionLeafItem"
              ∧ tt "laundrycommon/commonclass/accordion/AccordionLeafItem"
              )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ tt
                  "laundrycommon/commonclass/accordion/AccordionMenu.leafMap:Ljava/util/TreeMap;"
              )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ tt
                  "laundrycommon/commonclass/accordion/AccordionMenu.leafMap:Ljava/util/TreeMap;"
              )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ ( tt
                    "laundrycommon/commonclass/accordion/AccordionRootItem<S]laundrycommon/commonclass/accordion/AccordionItem"
                ∧ tt
                    "laundrycommon/commonclass/accordion/AccordionItem<S]javax/swing/JLabel"
                ∨ tt
                    "laundrycommon/commonclass/accordion/AccordionRootItem<S]laundrycommon/commonclass/accordion/AccordionItem"
                ∧ tt
                    "laundrycommon/commonclass/accordion/AccordionItem<S]javax/swing/JLabel"
                )
              )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ tt
                  "laundrycommon/commonclass/accordion/AccordionRootItem.getBranchPanel:()Llaundrycommon/commonclass/accordion/AccordionBranch;"
              )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ ( tt
                    "laundrycommon/commonclass/accordion/AccordionBranch<S]javax/swing/JPanel"
                ∨ tt
                    "laundrycommon/commonclass/accordion/AccordionBranch<S]javax/swing/JPanel"
                )
              )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ tt
                  "laundrycommon/commonclass/accordion/AccordionMenu.selectionColor:Ljava/awt/Color;"
              )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ tt
                  "laundrycommon/commonclass/accordion/AccordionMenu.leafMap:Ljava/util/TreeMap;"
              )
            ∧ ( not
                  (tt
                    "laundrycommon/commonclass/accordion/AccordionMenu.setBackground:(Ljava/awt/Color;)V!code"
                  )
              ∨ ( tt
                    "laundrycommon/commonclass/accordion/AccordionLeafItem<S]laundrycommon/commonclass/accordion/AccordionItem"
                ∧ tt
                    "laundrycommon/commonclass/accordion/AccordionItem<S]javax/swing/JLabel"
                ∨ tt
                    "laundrycommon/commonclass/accordion/AccordionLeafItem<S]laundrycommon/commonclass/accordion/AccordionItem"
                ∧ tt
                    "laundrycommon/commonclass/accordion/AccordionItem<S]javax/swing/JLabel"
                )
              )
      let (nnf, x) = memorizeNnf ex
      let cnf = toMinimalCNF (maxVariable nnf) nnf
      debugCnfWith (\i -> maybe (shows i) showString (x V.!? i))  cnf
      debugCnfWith (\i -> maybe (shows i) showString (x V.!? i)) (toCNF nnf)

  describe "possitive progression" $ do
    it "caluclate it on a small case" $ do
      let
        ex = toCNF
          $ and [ff 1 \/ tt 0, ff 2 \/ tt 3 \/ tt 1, ff 3 \/ tt 2 :: Nnf Int]
      -- debugCnf ex
      let
        _ = weightedSubDisjunctions (fromIntegral . IS.size)
                                    (fromJust $ fromCNF ex)
      -- putStrLn ""
      -- mapM_ (\(c, x) -> do print c; debugIpf x; putStrLn "") a
      True `shouldBe` True

    it "run it on a real case" $ do
      Just (ex :: Nnf Text.Text) <-
        fmap and . sequence . map decode . BLC.lines <$> BL.readFile
          "test/data/main-example.json"
      let (nnf, _) = memorizeNnf ex
      let cnf      = toMinimalCNF (maxVariable nnf) nnf
      let
        _ = weightedSubDisjunctions (fromIntegral . IS.size)
                                    (fromJust $ fromCNF cnf)
      True `shouldBe` True



      -- let (cnf', lup') = compressCNF IS.empty (fromIntegral . IS.size) cnf
      -- print lup'
      -- debugCnf cnf'

      -- let terms = positiveProgression (cnfVariables cnf') cnf'
      -- print terms

      -- forM_ [[lup V.! l' |  l' <- IS.toList (lup' V.! l) ] | t <- terms, l <- IS.toList t] print
      -- print [ (map (map (lup' V.!) . IS.toList) terms ]

    -- it "can caluclate graph" $ do
    --   let ex = toCNF $ and 
    --         [ ff 1 \/ tt 0 
    --         , ff 2 \/ tt 3 \/ tt 1
    --         , ff 3 \/ tt 2  :: Nnf Int 
    --         ]
    --   let (_, cnf') = compressCNF IS.empty (fromIntegral . IS.size) ex
    --   (v, cnf') `shouldBe` (v, cnf')






