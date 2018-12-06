{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : System.Directory.Tree
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

A module roughly based on the directory-tree package, but
is a little easier to work with.
-}
module System.Directory.Tree
  ( DirTree

  , AnchoredTree (..)
  , foldTreeWithFilePath
  , toFileList
  , fromFileList

  , readTree
  , writeTreeWith

  , FileContent (..)
  , writeContent

  ) where

-- containers
import qualified Data.Map         as Map

-- deepseq
import           Control.DeepSeq

-- data-fix
import           Data.Fix

-- directory
import           System.Directory

-- filepath
import           System.FilePath

-- bytestring
import qualified Data.ByteString.Lazy                  as BL
-- import qualified Data.ByteString.Lazy.Char8            as BLC

-- base
import           Data.Monoid
import           Data.Either
import qualified Data.List as List
import           GHC.Generics

-- | A directory tree node.
data DirTreeNode a r
  = File !a
  | Dir !r
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

foldDirTreeNode :: (a -> b) -> (r -> b) -> DirTreeNode a r -> b
foldDirTreeNode ff fd =
  \case
    File a -> ff a
    Dir  r -> fd r

newtype DirTreeF a r =
  DirTreeF { unDirTreeF :: Map.Map FilePath (DirTreeNode a r) }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

type DirTree' a = Fix (DirTreeF a)

-- | A directory tree
newtype DirTree a =
  DirTree {unDirTree :: Fix (DirTreeF a)}
  deriving (Show, Eq, Ord, Generic)

-- file :: a -> DirTree a
-- file = fromNode . File
-- {-# INLINE file #-}

-- dir :: Map.Map FilePath (DirTree a) -> DirTree a
-- dir = fromNode . Dir . coerce
-- {-# INLINE dir #-}

-- -- (Map.Map FilePath (Fix (DirTreeF a)))
-- fromNode :: DirTreeNode a (DirTree) -> DirTree a
-- fromNode = DirTree . Fix . DirTreeF
-- underDir f = DirTree . f . unDirTree
-- {-# INLINE underDir #-}

-- cataD :: (DirTreeNode a (Fix (DirTreeF a)) -> c) -> DirTree a -> c
-- cataD f = cata (fmap f . unDirTreeF) . unDirTree

instance Functor DirTree where
  fmap f =
    DirTree . cata (Fix . DirTreeF . Map.map node . unDirTreeF) . unDirTree
    where
      node = \case
        File a -> File (f a)
        Dir b -> Dir b

instance Foldable DirTree where
  foldMap f =
    cata (foldMap (foldDirTreeNode f id) . unDirTreeF) . unDirTree

instance Traversable DirTree where
  traverse f (DirTree m) = DirTree <$> cata go m
    where
      go (DirTreeF m') = Fix . DirTreeF <$> traverse help m'
      help = foldDirTreeNode (fmap File . f) (fmap Dir)

data AnchoredTree a = (:/)
  { anchor  :: ! FilePath
  , dirTree :: ! (DirTree a)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

foldTreeWithFilePath ::
  Monoid m
  => (FilePath -> a -> m)
  -> (FilePath -> m)
  -> DirTree a
  -> m
foldTreeWithFilePath f d (DirTree tree) =
  go id tree
  where
    go fpm tree'  =
      flip Map.foldMapWithKey (unDirTreeF . unFix $ tree') $ \fp -> \case
        File a -> f (fpm fp) a
        Dir r -> d (fpm fp) <> go (fpm fp </>) r

toFileList :: DirTree a -> [(FilePath, a)]
toFileList tree =
  let x = foldTreeWithFilePath (\fp a -> Endo ((fp, a):)) (const mempty) tree
  in appEndo x []

fromFileList :: [(FilePath, a)] -> Maybe (DirTree a)
fromFileList lst =
  fmap DirTree . group $ map (\(fp, a) -> (splitDirectories fp, a)) lst
  where
    group :: [([String], a)] -> Maybe (DirTree' a)
    group grp =
      case partitionEithers $ map unwind grp of
        ([], rest) ->
          Fix . DirTreeF <$> traverse casx (Map.fromListWith (++) rest)
        _ ->
          Nothing

    casx [([], a)] = Just $ File a
    casx res = Dir <$> group res

    unwind (x, a) =
      maybe (Left a) (\(f,rest) -> Right (f, [(rest, a)])) $ List.uncons x

readTree :: FilePath -> IO (AnchoredTree FilePath)
readTree fp = do
  tree <- DirTree <$> anaM go fp
  return $ fp :/ tree
  where
    go fp' = do
      files <- listDirectory fp'
      DirTreeF . Map.fromList <$> mapM (readDir fp') files

    readDir :: FilePath -> FilePath -> IO (FilePath, DirTreeNode FilePath FilePath)
    readDir fp' f = do
      let f' = fp' </> f
      x <- doesDirectoryExist f'
      return . (f,) $ if x
        then Dir f'
        else File f'

writeTreeWith :: (FilePath -> a -> IO ()) -> AnchoredTree a -> IO ()
writeTreeWith f (fp :/ tree) = do
  createDirectoryIfMissing True fp
  foldTreeWithFilePath handleFile handleDirectory  tree

  where
    handleFile f' a = do
      f (fp </> f' ) a
    handleDirectory f' = do
      createDirectory (fp </> f')

data FileContent
  = Content BL.ByteString
  | SameAs FilePath
  deriving (Show, Eq)

writeContent :: FilePath -> FileContent -> IO ()
writeContent fp = \case
  Content bs ->
    BL.writeFile fp bs
  SameAs old ->
    createFileLink old fp
