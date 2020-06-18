{-# LANGUAGE BlockArguments #-}
module Control.Reduce.ProgressionSpec where

import SpecHelper

-- hspec
import Test.Hspec.Hedgehog
-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- base
import Data.Foldable
import Control.Monad
import Data.Maybe
import qualified Data.List as L

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BS

-- -- vector
-- import qualified Data.Vector as V

-- conainers
import qualified Data.Tree as T
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import qualified Data.IntSet as IS


-- reduce-util
import qualified Control.Reduce.Graph          as G
import Control.Reduce.Progression
import Control.Reduce.Boolean
import Control.Reduce.Boolean.CNF as CNF hiding (progression)
import Control.Reduce.Boolean.LiteralSet as LS


genIpf :: MonadGen m => Int -> m CNF
genIpf vars = do
  fmap CNF . Gen.set (Range.linear 0 (vars-1)) $
    genPositiveClause vars


genPositiveClause :: MonadGen m => Int -> m Clause
genPositiveClause vars = do
  s <- S.toList <$> Gen.set (Range.exponential 1 (vars -1)) (genVar vars)
  t <- Gen.int (Range.linear 0 (length s -2))
  (ff,tt) <- splitAt t <$> Gen.shuffle s
  let Just l = joinLiterals (IS.fromList ff, IS.fromList tt)
  return l


genVar :: MonadGen m => Int -> m Int
genVar vars = Gen.int (Range.linear 0 (vars -1))

genGraph :: MonadGen m => Int -> m (G.Graph () Int)
genGraph size = do
  fmap (fst . G.buildGraph') . forM [0 .. size -1] $ \n -> do
    ns <- filterM (const (Gen.bool)) [0 .. size -1]
    return (n, ns)

genIpfFromGraph :: MonadGen m => Int -> m (G.Graph () Int, CNF)
genIpfFromGraph size = do
  graph <- genGraph size
  trues <- filterM (const Gen.bool) [0 .. size -1]
  pure $ (graph, CNF (S.fromList $
    [LS.singleton (tt t) | t <- trues]
    ++ mapMaybe edgeToClause (G.edges graph)
    ))
 where
  edgeToClause (G.Edge () e1 e2) =
    fromList [ ff e1, tt e2 ]


spec :: Spec
spec = do
  describe "progression" do
    it "is a split" . hedgehog $ do
      nv <- forAll $ Gen.int (Range.exponential 1 100)
      ipf <- forAll (genIpf nv)
      let vs = IS.fromList [0..nv-1]
      IS.unions (calculateProgression generateTotalGraphOrder ipf vs)
        === vs

    it "is all models" . hedgehog $ do
      nv <- forAll $ Gen.int (Range.exponential 1 100)
      ipf <- forAll (genIpf nv)
      let vs = IS.fromList [0..nv-1]
      let d = calculateProgression generateTotalGraphOrder ipf vs
      forM_ (tail $ L.inits (NE.toList d)) \di -> do
        annotateShow di
        let ipf' = conditionCNF (IS.unions di) ipf
        annotateShow ipf'
        assert $ isDualIPF ipf'

    closures

closures = it "on graphs it only produce closures" . hedgehog $ do
  nv <- forAll $ Gen.int (Range.linear 1 5)
  (graph, ipf) <- forAll (genIpfFromGraph nv)

  let vs = IS.fromList [0..nv-1]
  let d  = calculateProgression generateTotalGraphOrder ipf vs

  forM_ (NE.tail d) \di ->
    assert (di `L.elem` S.fromList (G.scc graph))

  NE.head d === IS.fromList (concatMap T.flatten (G.dfs graph (IS.toList $ IS.unions (nonNegativeClausesVariables ipf))))



