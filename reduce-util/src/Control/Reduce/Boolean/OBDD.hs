{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Based loosly on Henrik Reif Andersen's "An Introduction to Binary Decision
-- Diagrams"
module Control.Reduce.Boolean.OBDD where

-- base
import Prelude hiding (not)
import Data.Bool hiding (not)
import GHC.Generics (Generic)
import Data.STRef
import Control.Monad.ST 

-- text
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder

-- hashtables
import qualified Data.HashTable.ST.Cuckoo as CHT
import qualified Data.HashTable.Class as HT

-- hashable
import Data.Hashable

-- reduce
import Control.Reduce.Boolean

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- | A Bdd Entry
data BddE = BddE
  { bddVar  :: {-# UNPACK #-} !Int
  , bddLow  :: {-# UNPACK #-} !Int
  , bddHigh :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord, Generic)

instance Hashable BddE where
  s `hashWithSalt` BddE {..} =
    s `hashWithSalt` bddVar
      `hashWithSalt` bddLow
      `hashWithSalt` bddHigh

-- | An Obdd.
data Obdd = Obdd
  { obddEntries :: !(V.Vector (Int, Int, Int))
  , obddTop     :: !Int
  , obddTrue    :: !Int
  , obddFalse   :: !Int
  } deriving (Show, Eq)

lookupObddEntry :: Obdd -> Int -> BddE
lookupObddEntry Obdd {..} i =
  let (x, y, z) = obddEntries V.! i
  in BddE x y z

runObbdBuilder :: (forall s. (BddE -> ST s Int) -> ST s Int) -> Obdd
runObbdBuilder fn = runST $ do
  hashmap  <- HT.new
  countVar <- newSTRef 0
  itemsVar <- newSTRef =<< VM.new 128

  obddTop <- fn \new -> CHT.mutateST hashmap new \case
    Just k ->
      return (Just k, k)
    Nothing -> do
      count <- readSTRef countVar
      writeSTRef countVar (count+1)
      itemsM <- readSTRef itemsVar
      itemsM' <- if VM.length itemsM <= count
        then do
          itemsM' <- VM.grow itemsM count
          writeSTRef itemsVar itemsM'
          return itemsM'
        else return itemsM
      VM.unsafeWrite itemsM' count (let BddE x y z = new in (x, y, z))
      return (Just count, count)

  itemsM <- readSTRef itemsVar
  obddEntries <- V.take (obddTop+1) <$> V.freeze itemsM

  let
    obddFalse = negate 2
    obddTrue = negate 1

  return $ Obdd { obddEntries, obddTop, obddTrue, obddFalse }
{-# INLINEABLE runObbdBuilder #-}

apply :: (Bool -> Bool -> Bool) -> Obdd -> Obdd -> Obdd
apply op o1 o2 = runObbdBuilder \create -> do
  hashmap <- HT.new
  let
    app u1 u2
      | u1 < 0 /\ u2 < 0 =
        return . bool (negate 2) (negate 1)
        $ op (u1 == obddTrue o1) (u2 == obddTrue o2)
      | otherwise = do
        CHT.mutateST hashmap (u1, u2) \case
          Just r  -> return (Just r, r)
          Nothing
            | u1 < 0 -> handle do
                let BddE v2 l2 h2 = o2 `lookupObddEntry` u2
                BddE v2 <$> app u1 l2 <*> app u1 h2
            | u2 < 0 -> handle do
                let BddE v1 l1 h1 = o1 `lookupObddEntry` u1
                BddE v1 <$> app l1 u2 <*> app h1 u2
            | otherwise -> handle do
                let
                  BddE v1 l1 h1 = o1 `lookupObddEntry` u1
                  BddE v2 l2 h2 = o2 `lookupObddEntry` u2
                case v1 `compare` v2 of
                  EQ -> BddE v1 <$> app l1 l2 <*> app h1 h2
                  LT -> BddE v1 <$> app l1 u2 <*> app h1 u2
                  GT -> BddE v2 <$> app u1 l2 <*> app u1 h2
          where
            handle m = do
              a@(BddE _ l h) <- m
              r <- if l /= h then create a else return l
              return (Just r, r)

  app (obddTop o1) (obddTop o2)
{-# INLINEABLE apply #-}

instance Boolean Obdd where
  true  = Obdd V.empty (negate 1) (negate 1) (negate 2)
  false = Obdd V.empty (negate 2) (negate 1) (negate 2)
  (/\) = apply (/\)
  (\/) = apply (\/)
  not a = a { obddTrue = obddFalse a, obddFalse = obddTrue a }

compileObdd :: Term Int -> Obdd
compileObdd = cata \case
  TAnd a b ->
    apply (/\) a b
  TOr a b ->
    apply (\/) a b
  TVar l -> Obdd (V.singleton (l, -2, -1)) 0 (-1) (-2)
  TNot a -> not a
  TConst True -> true
  TConst False -> false

-- | Print a reducedNnf as a dot graph
dotObdd :: Obdd -> LazyText.Text
dotObdd Obdd {..} =
  toLazyText $ "digraph {\n "
    <> key obddFalse <> " [label=\"0\",shape=box];\n "
    <> key obddTrue <> " [label=\"1\",shape=box];\n "
    <> V.ifoldl' (\a i b -> a <> handle i b) mempty obddEntries
    <> "}"
  where
    key i
      | i < 0 =
        "b" <> fromString (show (abs i))
      | otherwise =
        "v" <> fromString (show i)
    handle i (v, l, h) = do
      key i <> " [label=\"" <> fromString (show v) <> "\"];\n "
        <> key i <> " -> " <> key l <> "[style=dashed];\n "
        <> key i <> " -> " <> key h <> ";\n "


