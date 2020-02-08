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

-- base
import Control.Monad.ST 
import Data.STRef
import Data.Semigroup
import Text.Show
import Data.Maybe
import Data.Traversable
import Data.Foldable
import qualified Data.List as L

-- containers
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

-- reduce-util
import Control.Reduce.Boolean

-- | A Clause is a disjunction of literals. This can be 
-- represented as an `IS.IntSet` of variable ids. The 
-- negative literals are the variable ids added with the minimum bound.   
newtype Clause = Clause { clauseSet :: IS.IntSet }
  deriving Eq  via IS.IntSet 
  deriving Ord via IS.IntSet 
  deriving Semigroup via IS.IntSet
  deriving Monoid via IS.IntSet

emptyClause :: Clause
emptyClause = Clause IS.empty

instance Show Clause where
  showsPrec n c = 
    showParen (n > 9) (showString "unsafeFromLiterals " . (showListWith shows $ toLiterals c))

splitClause :: Clause -> (IS.IntSet, IS.IntSet)
splitClause (Clause is) =
  (IS.fromDistinctAscList . L.map (\a -> a - minBound) $ IS.toDescList falses, trues)
  where (falses, trues) = IS.split (negate 1) is

{-# INLINE splitClause #-}

clauseVariables :: Clause -> IS.IntSet
clauseVariables = view both . splitClause 

clauseToLiterals :: Clause -> [Literal Int]
clauseToLiterals c = 
  L.map (Literal False) (IS.toAscList falses)
  <> L.map (Literal True) (IS.toAscList trues)
  where (falses, trues) = splitClause c

clauseAddLiteral :: Literal Int -> Clause -> Maybe Clause
clauseAddLiteral (Literal b v) (Clause is) 
  | b     = 
    if v' `IS.member` is 
    then Nothing
    else Just $ Clause (v `IS.insert` is)
  | otherwise = 
    if v `IS.member` is 
    then Nothing
    else Just $ Clause (v' `IS.insert` is)
  where 
    v' = v + minBound

clauseFromLiterals :: Foldable t => t (Literal Int) -> Maybe Clause
clauseFromLiterals = foldrM clauseAddLiteral emptyClause

unsafeClauseFromLiterals :: Foldable t => t (Literal Int) -> Clause
unsafeClauseFromLiterals = fromJust . fromLiterals

displayImplication :: (Int -> ShowS) -> Clause -> ShowS
displayImplication showKey c =
  showItems " /\\ " falses . showString " ==> " . showItems " \\/ " trues
 where 
   (falses, trues) = splitClause c
   showItems del =
     appEndo . foldMap Endo . L.intersperse (showString del) . map showKey . IS.toList 


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
    NLit l -> maybe S.empty S.singleton . clauseAddLiteral l
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
        ys <- b (fromJust $ clauseAddLiteral (tt i) d)
        pure (ys <> S.map (fromJust . clauseAddLiteral (ff i)) xs)
      else do
        ys <- mapM b (S.toList xs)
        pure $ S.unions ys

    NLit l -> pure . maybe S.empty S.singleton . clauseAddLiteral l
    NConst True -> pure . const S.empty
    NConst False -> pure . S.singleton 

   where
     freshvar = do
      a <- readSTRef nextid
      writeSTRef nextid (a + 1) 
      return a


cnfVariables :: CNF -> IS.IntSet
cnfVariables =
  foldMap clauseVariables . cnfClauses

debugCnf :: CNF -> IO ()
debugCnf cnf = 
  forM_ (cnfClauses cnf) \a -> putStrLn (displayImplication shows a "")

cnfSize :: CNF -> Int
cnfSize = S.size . cnfClauses

newtype Term = Term { termSet :: IS.IntSet }
  deriving Eq  via IS.IntSet 
  deriving Ord via IS.IntSet 
  deriving Semigroup via IS.IntSet
  deriving Monoid via IS.IntSet

emptyTerm :: Term
emptyTerm = Term IS.empty

instance Show Term where
  showsPrec n c = 
    showParen (n > 9) (showString "unsafeFromLiterals " . (showListWith shows $ toLiterals c))

termAddLiteral :: Literal Int -> Term -> Maybe Term
termAddLiteral (Literal b v) (Term is) 
  | b     = 
    if v `IS.member` is 
    then Nothing
    else Just $ Term (v `IS.insert` is)
  | otherwise = 
    if v' `IS.member` is 
    then Nothing
    else Just $ Term (v' `IS.insert` is)
  where 
    v' = v + minBound

termFromLiterals :: Foldable t => t (Literal Int) -> Maybe Term
termFromLiterals = foldrM termAddLiteral emptyTerm

splitTerm :: Term -> (IS.IntSet, IS.IntSet)
splitTerm (Term is) =
  (IS.fromDistinctAscList . L.map (\a -> a - minBound) $ IS.toDescList falses, trues)
  where (falses, trues) = IS.split (negate 1) is

termToLiterals :: Term -> [Literal Int]
termToLiterals c = 
  L.map (Literal False) (IS.toAscList falses)
  <> L.map (Literal True) (IS.toAscList trues)
  where (falses, trues) = splitTerm c


-- | Forward propergation of positive variables.
unitPropergation :: Term -> CNF -> (Term, CNF)
unitPropergation (termSet -> target) cnf = runST $ do
  current <- V.thaw (V.map Just clauses)
  results <- newSTRef target
  let 
    go left = case IS.minView left of
      Just (x, rest) -> do
        xs <- IS.unions <$> forM (IS.toList $ clauseLookup IM.! x) \i -> do
          VM.read current i >>= \case
            Nothing -> return $ IS.empty
            Just (condition x -> clause) -> do
              VM.write current i clause
              case clause of 
                Just (clauseSet -> s) 
                  | IS.size s == 1 -> return s
                _ -> return IS.empty

        lastResults <- readSTRef results
        writeSTRef results (lastResults `IS.union` xs)
        go (rest `IS.union` (xs `IS.difference` lastResults))

      Nothing -> return ()
    
  go target

  after <- readSTRef results 
  cnfResult <- V.freeze current
  return (Term after, CNF . fold . V.mapMaybe (fmap S.singleton) $ cnfResult)

 where
  condition x (Clause s) =
    if x < 0 
    then 
      if x `IS.member` s
      then Nothing
      else Just . Clause $ (x + minBound) `IS.delete` s 
    else
      if x `IS.member` s
      then Nothing
      else Just . Clause $ (x - minBound) `IS.delete` s
    
  clauses = V.fromList (S.toList . cnfClauses $ cnf)
  clauseLookup = IM.unionsWith (<>) $ V.indexed clauses <&> \(i, c) -> 
    IM.fromSet (const $ IS.singleton i) (clauseVariables c)


class LiteralSet a where
  type LiteralSetType a
  toLiterals   :: a -> [Literal (LiteralSetType a) ]
  fromLiterals :: Foldable t => t (Literal (LiteralSetType a)) -> Maybe a

unsafeFromLiterals :: (Foldable t, LiteralSet a) => t (Literal (LiteralSetType a)) -> a 
unsafeFromLiterals = fromJust . fromLiterals

instance LiteralSet Clause where
  type LiteralSetType Clause = Int
  toLiterals = clauseToLiterals
  fromLiterals = clauseFromLiterals

instance LiteralSet Term where
  type LiteralSetType Term = Int
  toLiterals = termToLiterals
  fromLiterals = termFromLiterals



-- toCNF :: (Fixed (NnfF a) b, Show a, Ord a) => b -> S.Set (S.Set a, S.Set a)
-- toCNF f =
--   cata cnf f (S.empty, S.empty)
--  where 
--   cnf 
--     :: (Show a, Ord a) => NnfF a ((S.Set a, S.Set a) -> S.Set (S.Set a, S.Set a)) 
--     -> (S.Set a, S.Set a) 
--     -> S.Set (S.Set a, S.Set a)
--   cnf = \case
--     NAnd a b -> \d -> a d <> b d
--     NOr a b -> \d -> S.fromList 
--       [ y 
--       | x <- S.toList (a d)
--       , y <- S.toList (b x)
--       ]
--     NLit (Literal n a) -> \(falses, trues) ->
--       case n of 
--         True  -> checkSat (falses, S.insert a trues)
--         False -> checkSat (S.insert a falses, trues)
--     NConst True -> const S.empty
--     NConst False -> S.singleton 

--   checkSat (falses, trues) 
--     | falses `S.disjoint` trues = 
--       S.singleton (falses, trues)
--     | otherwise = S.empty 


-- displayAClause :: (a -> ShowS) -> (S.Set a, S.Set a) -> ShowS
-- displayAClause fn (falses, trues) =  
--   ( appEndo
--   . foldMap Endo
--   . L.intersperse (showString " /\\ ") 
--   . map fn
--   . S.toList 
--   $ falses )
--   .
--   showString " ---> "
--   . ( 
--   appEndo
--   . foldMap Endo
--   . L.intersperse (showString " \\/ ") 
--   . map fn
--   . S.toList 
--   $ trues
--   )

-- displayCNF :: Foldable t => t (IS.IntSet) -> ShowS
-- displayCNF =
--   appEndo
--   . foldMap Endo
--   . L.intersperse (showString " ")
--   . map displayClause
--   . toList

-- displayClause :: IS.IntSet -> ShowS
-- displayClause = showParen True
--   . appEndo
--   . foldMap Endo
--   . L.intersperse (showString " ")
--   . map displayLiteral
--   . IS.toList

--   where
--     displayLiteral i = case reindex i of
--       (True, l)  -> showsPrec 0 l
--       (False, l) -> showString "!" . showsPrec 0 l





-- varsOf :: (Foldable f, Ord a) => f (S.Set a, S.Set a) -> S.Set a
-- varsOf = foldMap (\(a,b) -> S.union a b)
-- 
-- condition :: Ord a => S.Set a 
--   -> S.Set (S.Set a, S.Set a) 
--   -> (S.Set a, S.Set (S.Set a, S.Set a))
-- condition a =
--   foldMap \case 
--     (tts, ffs) 
--       | a `S.disjoint` ffs ->
--         let tts' = tts `S.difference` a
--         in if S.null tts' && S.size ffs == 1 
--         then (ffs, S.empty)
--         else (S.empty, S.singleton (tts', ffs))
--       | otherwise ->
--         (S.empty, S.empty)
-- 
-- propergate :: Ord a => S.Set a -> S.Set (S.Set a, S.Set a) 
--   -> (S.Set a, S.Set (S.Set a, S.Set a))
-- propergate a m = 
--   let (a', m') = condition a m 
--   in if S.null a'
--   then (a' <> a, m')
--   else propergate (a' <> a) m'
-- 
-- splits :: Ord a => S.Set a -> S.Set (S.Set a, S.Set a) -> [(S.Set a, S.Set (S.Set a, S.Set a))]
-- splits = go 
--  where
--   go vs cnf = case S.lookupMin vs of 
--     Just p -> 
--       let (items', cnf') = makeSat (S.singleton p) cnf
--       in (items', cnf') : go (vs `S.difference` items') cnf'
--     Nothing -> 
--       []
-- 
--   makeSat items cnf = 
--     let (items', cnf') = propergate items cnf
--     in case S.lookupMin cnf' of
--       Just (trues, falses) 
--        | S.null trues ->
--          makeSat (S.insert (S.findMin falses) items') cnf'
--       _ -> 
--         (items', cnf')

