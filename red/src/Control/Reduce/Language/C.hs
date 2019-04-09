{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
{-|
Module      : Control.Reduce.Language.C
Description : Reduction of C
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

Reduction of C
-}
module Control.Reduce.Language.C
  ( parseCFilePre
  , printCFile

  , CRed (..)
  , cR
  , cEdges

  , statStats
  , statExprs
  , initExprs
  , attrExprs
  , declExprs

  , _RCStat
  , _RCDecl
  , _RCFunDef
  , _RCAsmExt

  , _CDeclExt
  , _CFDefExt
  , _CFunDeclr
  , _CFunDef
  , _CDeclr
  , _CDecl
  , _CTypeDef
  , _CTypeSpec
  , _CVar
  , _CExpr
  , _CReturn
  , _CGoto

  , debuglst
  ) where

-- base
import           Control.Monad
import           Data.Ord
import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NE
import           Data.Monoid
import           Debug.Trace


-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BLC

-- pretty
import qualified Text.PrettyPrint           as PP

-- containers
import qualified Data.Map.Strict            as Map

-- lens
import           Control.Lens
import           Data.Data.Lens             (uniplate)

-- language-c
import           Language.C

-- reduce-util
import           Control.Reduce.Graph
import           Control.Reduce.Reduction

data CRed
  = RCTranslUnit CTranslUnit
  | RCStat CStat

  | RCDecl CDecl
  | RCFunDef CFunDef
  | RCAsmExt (CStrLit, NodeInfo)
  deriving (Show)

makePrisms ''CRed


debuglst :: CRed -> [([Int], String)]
debuglst crd =
  crd ^@.. treeSubelements cR.to toString & traverse._1 %~ NE.toList

  where
    toString :: CRed -> String
    toString =
      \case
        RCTranslUnit _ -> "TOP"
        RCStat a -> PP.render . pretty $ a
        RCDecl a -> PP.render . pretty $ a
        RCFunDef a -> PP.render . pretty $ a
        RCAsmExt (a,b) -> PP.render . pretty $ CAsmExt a b



printCFile :: CRed -> BLC.ByteString
printCFile = \case
  RCTranslUnit c ->
   BLC.pack . PP.render . pretty $ c
  _ -> "xxx"

grab :: Functor f => Prism' a b -> (a -> f (Maybe a)) -> b -> f (Maybe b)
grab prsm pab =
  fmap (preview $ _Just.prsm) . pab . review prsm

fetch :: PartialReduction CExtDecl CRed
fetch fab = \case
  CDeclExt d -> fmap CDeclExt <$> grab _RCDecl fab d
  CFDefExt f -> fmap CFDefExt <$> grab _RCFunDef fab f
  CAsmExt s b -> fmap (uncurry CAsmExt) <$> grab _RCAsmExt fab (s, b)

compound :: PartialReduction CBlockItem CRed
compound fab = \case
  CBlockStmt d -> fmap CBlockStmt <$> grab _RCStat fab d
  CBlockDecl f -> fmap CBlockDecl <$> grab _RCDecl fab f
  CNestedFunDef f -> fmap CNestedFunDef <$> grab _RCFunDef fab f

cR :: Reduction CRed CRed
cR (pab :: CRed -> f (Maybe CRed)) = \case

  RCTranslUnit (CTranslUnit e x) -> RCTranslUnit <$> do
    e' <- (listR . fetch) pab e
    pure (CTranslUnit e' x)

  RCFunDef (CFunDef decsp cdec decs stat a) -> RCFunDef <$> do
    stat' <- handleStat stat
    pure (CFunDef decsp cdec decs stat' a)

  RCStat stat -> RCStat <$> handleStat stat

  RCDecl (CDecl decsp rst a) -> RCDecl <$> do
    pure (CDecl decsp rst a)

  a -> pure $ a

  where
    handleStat = \case
      CLabel i s atr a -> do
        s' <- handleStat s
        pure $ CLabel i s' atr a

      CCases e1 e2 s a -> do
        s' <- handleStat s
        pure $ CCases e1 e2 s' a

      CDefault s a -> do
        s' <- handleStat s
        pure $ CDefault s' a

      CCompound labels items n -> do
        items' <- (listR . compound) pab items
        pure $ CCompound labels items' n

      CIf e s ms n -> do
        s' <- handleStat s
        ms' <- (maybeR . grab _RCStat) pab ms
        pure $ CIf e s' ms' n

      CWhile e s b n -> do
        s' <- handleStat s
        pure $ CWhile e s' b n

      CFor ex e1 e2 s a -> do
        s' <- handleStat s
        pure $ CFor ex e1 e2 s' a

      a -> pure a

  -- RCExtDecl c ->
  --   RCExtDecl <$> case c of
  --     CFDefExt (CFunDef dec cder declrs stmt a) -> do
  --       stmt' <- fromMaybe (CReturn Nothing undefNode)
  --         <$> (fmap (preview $ _RCStat) . cR pab $ RCStat stmt)
  --       pure $ CFDefExt (CFunDef dec cder declrs stmt' a)
  --     _ ->

  -- RCStat c ->
  --   RCStat <$>
  --   case c of
  --     CCompound idents cmbi a -> do
  --       cmbi' <- listR (fmap (preview $ _Just._RCBlockItem). pab . RCBlockItem) cmbi
  --       pure $ CCompound idents cmbi' a
  --     _ -> pure c




cEdges :: CTranslUnit -> [Edge () [Int]]
cEdges ctu = do
  variableEdges ++ labelEdges
  where
    variableEdges = do
      (NE.toList -> fi, ident) <- func ++ stms ++ decl
      traceM $ "types: " ++ show (Map.toList types & traverse._1 %~ identToString)
      traceM $ "func: " ++ makeNice func
      traceM $ "stms: " ++ makeNice stms
      traceM $ "decl: " ++ makeNice decl
      case Map.lookup ident types of
        Just etos ->
          return $ Edge fi (getLargestPrefix fi etos) ()
        Nothing  -> mzero

    labelEdges = do
      traceM $ "labels: " ++ show (Map.toList labels & traverse._1 %~ identToString)
      traceM $ "usages: " ++ makeNice labelUsages
      (NE.toList -> fi, ident) <- labelUsages
      case Map.lookup ident labels of
        Just etos ->
          return $ Edge fi (getLargestPrefix fi etos) ()
        Nothing  -> mzero

    makeNice :: [(NE.NonEmpty Int, Ident)] -> String
    makeNice f =
      show ( f & traverse . _1 %~ NE.toList & traverse._2 %~ identToString )

    getLargestPrefix fi etos =
      L.maximumBy (
          comparing (\a ->
                       if L.isPrefixOf fi a || L.isPrefixOf a fi
                       then 0
                       else L.length . L.takeWhile id . zipWith (==) (reverse fi) . reverse $ a
                    )
          )
        etos

    types :: Map.Map Ident [[Int]]
    types =
      fmap (($ []) . appEndo)
      . Map.fromListWith (<>)
      . map (\(i, a) -> (a, Endo (NE.toList i:)))
      $ ctu' ^@..
       ( ( getting iters._RCDecl <.
           cosmosOf deepdecl.
          _CDecl.(
             _1.folded._CTypeSpec.(
                 _CSUType._1._CStruct._2._Just
             )
             <>
             _2.folded._1._Just._CDeclr._1._Just
             )
         ) <>
         ( getting iters._RCFunDef <.
           _CFunDef . _2._CDeclr._1._Just
         )
       )

    deepdecl :: Fold CDecl CDecl
    deepdecl =
      _CDecl._1.folded._CTypeSpec._CSUType._1._CStruct._3._Just.folded

    func = ctu' ^@..
      getting iters._RCFunDef._CFunDef <.
      ( _1.folded._TypeId
        <> _2._CDeclr._2
        .folded._CFunDeclr._1._Right
        ._1.folded._CDecl
        ._1.folded._TypeId
      )

    stms = ctu' ^@..
      getting iters._RCStat <. cosmosOnOf statExprs exprs . _CVar . _1

    decl = ctu' ^@..
      getting iters._RCDecl <.
      (cosmosOf deepdecl.
       ( _CDecl._1.folded._TypeId
       <> cosmosOnOf declExprs exprs._CVar._1
       )
      )

    _TypeId =
      _CTypeSpec.
      ( _CTypeDef._1
      <> _CSUType._1._CStruct._2._Just
      )

    labels =
      fmap (($ []) . appEndo)
      . Map.fromListWith (<>)
      . map (\(i, a) -> (a, Endo (NE.toList i:)))
      $ ctu' ^@.. getting iters._RCStat <. (_CLabel._1 <> _CCompund._1.traverse)

    labelUsages =
      ctu' ^@..
        getting iters._RCStat <. cosmosOf statStats._CGoto._1

    ctu' = RCTranslUnit ctu

    iters :: TreeReduction CRed
    iters = treeReduction cR

-- | Emidiate (non-reducable) childern of a CStat
statStats :: Traversal' CStat CStat
statStats fab =
  \case
    CLabel i s ats a -> do
      s' <- fab s
      pure $ CLabel i s' ats a
    CCase e s a -> do
      s' <- fab s
      pure $ CCase e s' a
    CCases e e' s a -> do
      s' <- fab s
      pure $ CCases e e' s' a
    CIf e s ms a -> do
      s' <- fab s
      pure $ CIf e s' ms a
    CSwitch e s a -> do
      s' <- fab s
      pure $ CSwitch e s' a
    CWhile e s b a -> do
      s' <- fab s
      pure $ CWhile e s' b a
    CFor ex e2 e3 s a -> do
      s' <- fab s
      pure $ CFor ex e2 e3 s' a
    -- CAsm
    a -> pure a

statExprs :: Traversal' CStat CExpr
statExprs fab =
  \case
    CLabel i s ats a -> CLabel i <$> statExprs fab s <*> (traverse.attrExprs) fab ats <*> pure a
    CCase e s a -> CCase <$> fab e <*> statExprs fab s <*> pure a
    CCases e e' s a -> CCases <$> fab e <*> fab e' <*> statExprs fab s <*> pure a
    CExpr e a -> CExpr <$> _Just fab e <*> pure a
    CIf e s s' a -> CIf <$> fab e <*> statExprs fab s <*> pure s' <*> pure a
    CSwitch e s a -> CSwitch <$> fab e <*> statExprs fab s <*> pure a
    CWhile e s b a -> CWhile <$> fab e <*> statExprs fab s <*> pure b <*> pure a
    CFor ex e2 e3 s a -> CFor <$> forHelper ex <*> _Just fab e2 <*> _Just fab e3 <*> statExprs fab s <*> pure a
    CGotoPtr e a -> CGotoPtr <$> fab e <*> pure a
    CReturn e a -> CReturn <$> _Just fab e <*> pure a
    -- CAsm
    a -> pure a

  where
    forHelper = \case
      Left a -> Left <$> _Just fab a
      Right a -> Right <$> declExprs fab a

declExprs :: Traversal' CDecl CExpr
declExprs fab =
  \case
    CDecl spc decs a -> do
      decs' <- (traverse.declHelper) fab decs
      pure $ CDecl spc decs' a
    CStaticAssert e c a -> do
      e' <- fab e
      pure $ CStaticAssert e' c a

  where
    declHelper fab' (a, b, c) =
      (,,)
      <$> pure a
      <*> (_Just.initExprs) fab' b
      <*> _Just fab' c

initExprs :: Traversal' CInit CExpr
initExprs fab =
  \case
    CInitExpr e a -> CInitExpr <$> fab e <*> pure a
    CInitList l a -> CInitList <$> initListExprs fab l <*> pure a

  where
    initListExprs :: Traversal' [([CDesignator], CInit)] CExpr
    initListExprs =
      traverse.(\fab' (a,b) -> (,) <$> (traverse.partDescExpr) fab' a <*> initExprs fab' b)

    partDescExpr fab' =
      \case
        CArrDesig e a -> CArrDesig <$> fab' e <*> pure a
        CRangeDesig e e' a -> CRangeDesig <$> fab' e <*> fab' e' <*> pure a
        a -> pure a

attrExprs :: Traversal' CAttr CExpr
attrExprs fab (CAttr i exps a) =
  CAttr i <$> traverse fab exps <*> pure a

exprs :: Fold CExpr CExpr
exprs = uniplate

  -- \case
  -- CComma es a -> CComma <$> folded fba es <*> pure a
  -- CAssign op ea eb a -> CAssign <$> folded fba es <*> pure a
  -- CCall a bs p -> CCall <$> fba a <*> folded fba bs <*> pure p
  -- a -> pure a

-- Prisms

_CDeclExt :: Prism' CExtDecl CDecl
_CDeclExt = prism' CDeclExt (\case CDeclExt a -> Just a; _ -> Nothing)

_CFDefExt :: Prism' CExtDecl CFunDef
_CFDefExt = prism' CFDefExt (\case CFDefExt a -> Just a; _ -> Nothing)

_CFunDeclr :: Prism' CDerivedDeclr (Either [Ident] ([CDecl], Bool), [CAttr], NodeInfo)
_CFunDeclr = prism' (\(a, b, c) -> CFunDeclr a b c) (\case CFunDeclr a b c -> Just (a, b, c); _ -> Nothing)

_CFunDef :: Iso' CFunDef ([CDeclSpec], CDeclr, [CDecl], CStat, NodeInfo)
_CFunDef = iso (\(CFunDef a b c d e) -> (a, b, c, d, e)) (\(a, b, c, d, e) -> CFunDef a b c d e)

_CStruct :: Iso' CStructUnion (CStructTag, Maybe Ident, Maybe [CDecl], [CAttr], NodeInfo)
_CStruct = iso (\(CStruct a b c d e) -> (a, b, c, d, e)) (\(a, b, c, d, e) -> CStruct a b c d e)

_CDeclr :: Iso' CDeclr (Maybe Ident, [CDerivedDeclr], Maybe CStrLit, [CAttr], NodeInfo)
_CDeclr = iso (\(CDeclr a b c d e) -> (a, b, c, d, e)) (\(a, b, c, d, e) -> CDeclr a b c d e)

_CDecl :: Prism' CDecl ([CDeclSpec], [(Maybe CDeclr, Maybe CInit, Maybe CExpr)], NodeInfo)
_CDecl = prism' (\(a, b, x) -> CDecl a b x) $
  \case CDecl a b x -> Just (a, b, x); _ -> Nothing

_CTypeDef :: Prism' CTypeSpec (Ident, NodeInfo)
_CTypeDef = prism' (\(i,x) -> CTypeDef i x) $ \case CTypeDef i a -> Just (i, a); _ -> Nothing

_CSUType :: Prism' CTypeSpec (CStructUnion, NodeInfo)
_CSUType = prism' (\(i,x) -> CSUType i x) $ \case CSUType i a -> Just (i, a); _ -> Nothing

_CEnumType :: Prism' CTypeSpec (CEnum, NodeInfo)
_CEnumType = prism' (\(i,x) -> CEnumType i x) $
  \case CEnumType i a -> Just (i, a); _ -> Nothing

_CTypeSpec :: Prism' CDeclSpec CTypeSpec
_CTypeSpec = prism' CTypeSpec $ \case CTypeSpec a -> Just a; _ -> Nothing

_CVar :: Prism' CExpr (Ident, NodeInfo)
_CVar = prism' (\(a, b) -> CVar a b) $ \case CVar a b -> Just (a, b); _ -> Nothing

_CExpr :: Prism' CStat (Maybe CExpr, NodeInfo)
_CExpr = prism' (\(a, b) -> CExpr a b) $ \case CExpr a b -> Just (a, b); _ -> Nothing

_CCompund :: Prism' CStat ([Ident], [CBlockItem], NodeInfo)
_CCompund = prism' (\(a, b, c) -> CCompound a b c) $ \case CCompound a b c -> Just (a, b, c); _ -> Nothing

_CLabel :: Prism' CStat (Ident, CStat , [CAttr], NodeInfo)
_CLabel = prism' (\(a, b, c, d) -> CLabel a b c d) $ \case CLabel a b c d -> Just (a, b, c, d); _ -> Nothing

_CReturn :: Prism' CStat (Maybe CExpr, NodeInfo)
_CReturn = prism' (\(a, b) -> CReturn a b) $ \case CReturn a b -> Just (a, b); _ -> Nothing

_CGoto :: Prism' CStat (Ident, NodeInfo)
_CGoto = prism' (\(a, b) -> CGoto a b) $ \case CGoto a b -> Just (a, b); _ -> Nothing

_CInitExpr :: Prism' CInit (CExpr, NodeInfo)
_CInitExpr = prism' (\(a, b) -> CInitExpr a b) $ \case CInitExpr a b -> Just (a, b); _ -> Nothing

_CInitList :: Prism' CInit (CInitList, NodeInfo)
_CInitList = prism' (\(a, b) -> CInitList a b) $ \case CInitList a b -> Just (a, b); _ -> Nothing
