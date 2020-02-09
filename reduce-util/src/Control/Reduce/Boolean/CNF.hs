{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Reduce.Boolean.CNF where

-- lens
import Control.Lens

-- vector 
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- primitive
import Control.Monad.Primitive

-- base
import Control.Monad.ST 
import Data.STRef
import Data.Semigroup
import Data.Maybe
import Data.Foldable

-- containers
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

-- reduce-util
import Control.Reduce.Boolean
import qualified Control.Reduce.Boolean.LiteralSet as LS
import Control.Reduce.Boolean.LiteralSet (Clause, Term, IntLiteral, emptyClause)

-- | A CNF is a set of literals. A literal is a variable id, and if it is 
-- negative it will be substracted 1000.
newtype CNF = CNF { cnfClauses :: S.Set Clause }
  deriving (Show)

toCNF :: Fixed (NnfF Int) b => b -> CNF
toCNF f =
  CNF $ cata cnf f emptyClause
 where 
  cnf = \case
    NAnd a b -> \d -> a d <> b d
    NOr a b -> \d -> S.fromList [ y | x <- S.toList (a d), y <- S.toList (b x)]
    NLit l -> maybe S.empty S.singleton . LS.insert (LS.fromLiteral l)
    NConst True -> const S.empty
    NConst False -> S.singleton 
 
maxVariable :: Fixed (NnfF Int) b => b -> Int
maxVariable = cata \case 
  NAnd a b -> max a b
  NOr a b  -> max a b
  NLit (Literal _ a)  -> a
  _ -> 0

toMinimalCNF :: Fixed (NnfF Int) b => Int -> b -> CNF
toMinimalCNF maxvar f =
  CNF $ runST $ do 
    next <- newSTRef (maxvar + 1)
    cata (cnf next) f emptyClause
 where 
  cnf nextid = \case
    NAnd a b -> \d -> do 
     x <- a d 
     y <- b d
     pure (x <> y)
    NOr a b -> \d -> do 
      xs <- a d
      if S.size xs > 1 
      then do
        i <- freshvar 
        ys <- b (fromJust $ tt i `LS.insert` d)
        pure (ys <> S.map (fromJust . LS.insert (ff i)) xs)
      else do
        ys <- mapM b (S.toList xs)
        pure $ S.unions ys

    NLit l -> pure . maybe S.empty S.singleton . LS.insert (LS.fromLiteral l)
    NConst True -> pure . const S.empty
    NConst False -> pure . S.singleton 

   where
     freshvar = do
      a <- readSTRef nextid
      writeSTRef nextid (a + 1) 
      return a


cnfVariables :: CNF -> IS.IntSet
cnfVariables =
  foldMap LS.variables . cnfClauses

debugCnf :: CNF -> IO ()
debugCnf cnf = 
  forM_ (cnfClauses cnf) \a -> putStrLn (LS.displayImplication shows a "")

cnfSize :: CNF -> Int
cnfSize = S.size . cnfClauses

-- | Forward propergation of positive variables.
unitPropergation :: Term -> CNF -> (Maybe Term, CNF)
unitPropergation target cnf = runST $ do
  current <- V.thaw (V.map Just clauses)
  after <- unitResolutionM current 
    (\i -> IM.findWithDefault [] (variable . LS.toLiteral $ i) clauseLookup)
    target
  cnfResult <- V.freeze current
  return (after, CNF . fold . V.mapMaybe (fmap S.singleton) $ cnfResult)
 where
  clauses = V.fromList . S.toList . cnfClauses $ cnf
  clauseLookup = clauseLookupMap clauses 

unitResolutionM :: 
  PrimMonad m 
  => VM.MVector (PrimState m) (Maybe Clause) 
  -- ^ The vector of clauses
  -> (IntLiteral -> [Int])
  -- ^ The variable to occurence table
  -> Term
  -> m (Maybe Term)
unitResolutionM clauses clauseLookup t = go t (LS.toList t, []) where 
  go visited = \case 
    (x:xs, as) -> do
      learned <- mapMaybe (>>= LS.unSingleton) 
        <$> mapM (conditionClauseM clauses x) (clauseLookup x)
      case foldrM LS.insert visited learned of
        Nothing -> return Nothing
        Just visited' -> go visited' (xs, filter (`LS.notMember` visited) learned ++ as)
    ([],[]) -> return (Just visited)
    ([],as) -> go visited (reverse as, [])
{-# INLINE unitResolutionM #-}

-- | Condition a clause with a IntLiteral
conditionClauseM :: 
  PrimMonad m 
  => VM.MVector (PrimState m) (Maybe Clause) 
  -> IntLiteral
  -- ^ Clause Id
  -> Int
  -> m (Maybe Clause)
conditionClauseM clauses il i = VM.read clauses i >>= \case
  Nothing -> return Nothing
  Just (LS.conditionClause il -> clause) -> do
    VM.write clauses i clause
    return clause

clauseLookupMap :: V.Vector Clause -> IM.IntMap [Int]
clauseLookupMap clauses = 
  IM.map (flip appEndo []) . IM.unionsWith (<>) 
    $ V.indexed clauses <&> \(i, c) -> 
    IM.fromSet (const $ Endo (i:)) (LS.variables c)

-- -- Given a set of variables, and a CNF calculate a 
-- forwardDPLL :: IS.IntSet -> CNF -> [Term]
-- forwardDPLL vs cnf = do go where 
--   go vs cnf = case IS.minView vs of 
--     Just (p, _) -> 
--       let (term, clauses') = resolve (S.singleton p) clauses
--       in term : go (vs `S.difference` termVariables term) 
--     Nothing -> []
-- 
--   -- Find something that satisfy the results
--   resolve clauses = runST $ do 
--     let clauseLookup = clausesLookupMap clauses
--     undefined
-- 
--   clauses = V.fromList . S.toList . cnfClauses $ cnf
