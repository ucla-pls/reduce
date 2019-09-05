{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Control.Reduce.Metric where

-- base
import           GHC.Generics                     (Generic)
import           Data.Functor
import           Control.Monad.IO.Class

-- bytestring
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Char8       as BLC

-- cassava
import Data.Csv                         as C

-- time
import           Data.Time

-- lens
import           Control.Lens hiding ((.=))

-- intset
import qualified Data.IntSet as IS

-- text
import qualified Data.Text.Lazy.Builder           as Builder

-- reduce-util
import           Control.Reduce.Command
import qualified Control.Reduce.Util.Logger       as L

-- | Write the header of metric to a file.
writeHeader :: MonadIO m => AnyMetric s -> FilePath -> m ()
writeHeader metric fp =
  liftIO . BLC.writeFile fp $ headerString metric

-- | Append a row to a file
writeRow :: MonadIO m => AnyMetric s -> FilePath -> MetricRow s -> m ()
writeRow metric fp s =
  liftIO . BLC.appendFile fp $ metricRowString metric s

type MetricOut s =
  MetricRow s -> IO ()

setupMetric :: AnyMetric s -> FilePath -> IO (MetricOut s)
setupMetric metric fp = do
  writeHeader metric fp
  return $ \s -> writeRow metric fp s


-- * Metrics
class Metric a where
   order :: Const [BS.ByteString] a
   fields :: a -> [(BS.ByteString, BS.ByteString)]
   displayMetric :: a -> Builder.Builder

data Judgment
  = Success
  | Failure
  | Timeout
  | Bad
  deriving (Show, Eq)

showJudgment :: Judgment -> String
showJudgment = \case
  Success -> "success" :: String
  Failure -> "failure"
  Bad -> "bad"
  Timeout -> "timeout"

instance ToField Judgment where
  toField = toField . showJudgment

data MetricRow a = MetricRow
  { metricRowContent  :: !a
  , metricRowTime     :: !NominalDiffTime
  , metricRowFolder   :: !String
  , metricRowJudgment :: !Judgment
  , metricRowResults  :: !(Maybe (CmdResult (Maybe CmdOutputSummary)))
  } deriving (Show, Eq, Generic, Functor)

instance Metric a => C.DefaultOrdered (MetricRow a) where
  headerOrder _ =
    C.header $
    getConst (order :: Const [BS.ByteString] a)
    <> [ "folder"
       , "time"
       , "judgment"
       , "setup time"
       , "run time"
       , "status"
       , "stdout (length)"
       , "stdout (sha256)"
       , "stderr (length)"
       , "stderr (sha256)"
       ]

data Empty = E

instance ToField Empty where
  toField E = toField ("" :: String)

instance Metric a => C.ToNamedRecord (MetricRow a) where
  toNamedRecord MetricRow {..} =
    C.namedRecord $
    fields metricRowContent <>
    [ "folder" C..= metricRowFolder
    , "time" C..= (realToFrac metricRowTime :: Double)
    , "judgment" .= metricRowJudgment
    ] <>
    case metricRowResults of
      Nothing ->
        [ "setup time" .= E, "run time" .= E ] <> mOutputSummary Nothing
      Just results ->
        [ "setup time" C..= resultSetupTime results
        , "run time" C..= resultRunTime results
        ] <> mOutputSummary (resultOutput results)
    where

      mOutputSummary = \case
        Just o ->
          [ "status" .= (exitCodeToInt . outputCode $ o)
          , "stdout (length)" .= (snd . outputOut $ o)
          , "stdout (sha256)" .= (showHash . outputOut $ o)
          , "stderr (length)" .= (snd . outputErr $ o)
          , "stderr (sha256)" .= (showHash . outputErr $ o)
          ]
        Nothing ->
          [ "status" .= E
          , "stdout (length)" .= E
          , "stdout (sha256)" .= E
          , "stderr (length)" .= E
          , "stderr (sha256)" .= E
          ]



-- | Computes the metric
data AnyMetric s = forall p. Metric (MList p) => AnyMetric
  { computeMetric :: s -> MList p
  }

instance Contravariant AnyMetric where
  contramap st (AnyMetric f) = AnyMetric (f . st)

emptyMetric :: AnyMetric s
emptyMetric = AnyMetric $ const MNil

-- | Adds a metric to the list of metrics
addMetric :: Metric r => (s -> r) -> AnyMetric s -> AnyMetric s
addMetric sr (AnyMetric f) =
  AnyMetric (\s -> MCons (sr s) (f s))

-- | Compute a CSV Header
headerString :: AnyMetric s -> BL.ByteString
headerString (AnyMetric f)=
  case f of
    (_ :: s -> MList r) ->
      C.encodeDefaultOrderedByNameWith
      C.defaultEncodeOptions ([] :: [MetricRow (MList r)])

-- | Compute a row in a CSV file
metricRowString :: AnyMetric s -> MetricRow s -> BL.ByteString
metricRowString (AnyMetric f) row =
  C.encodeDefaultOrderedByNameWith
  ( C.defaultEncodeOptions { C.encIncludeHeader = False } )
  [ row $> f (metricRowContent row) ]

-- | Display the metric of a data point
displayAnyMetric :: AnyMetric s -> s -> Builder.Builder
displayAnyMetric (AnyMetric f) s =
  displayMetric (f s)

data MList k where
  MNil :: MList '[]
  MCons :: Metric a => a -> MList b -> MList (a ': b)

instance Metric (MList '[]) where
  order = Const []
  fields _ = []
  displayMetric _ = ""

instance (Metric a, Metric (MList as)) => Metric (MList (a ': as)) where
  order = Const (getConst (order :: Const [BS.ByteString] a) ++ getConst (order :: Const [BS.ByteString] (MList as)))
  fields (MCons a as) = fields a ++ fields as
  displayMetric (MCons a MNil) = displayMetric a
  displayMetric (MCons a as) = displayMetric a <> ", " <> displayMetric as

instance (Metric a, Metric b) => Metric (a, b) where
  order = Const (getConst (order :: Const [BS.ByteString] a) ++ getConst (order :: Const [BS.ByteString] b))
  fields (a, b) =
    fields a ++ fields b
  displayMetric (a, b) =
    displayMetric a <> " and " <> displayMetric b


counted :: String -> [b] -> Count
counted n =
  Count n . length

data Count = Count { itemsName :: !String,  getCount :: !Int }
  deriving (Show)

instance Metric Count where
  order = Const ["count"]
  fields (Count _ a) =
    ["count" C..= a ]
  displayMetric (Count n a) =
    L.displayf "#%i " a <> L.displayString n

stringified :: (Int -> Char) -> Int -> [Int] -> Stringify
stringified toChar len items =
  Stringify [ if IS.member i is then toChar i else '·' | i <- [0..len-1] ]
  where is = IS.fromList items

--   where
--     v = V.fromList b
--     str = V.map toChar v
--     idxs = V.toList . V.map fst . V.indexed $ v
--     displ a =
--       let is = IS.fromList a in

newtype Stringify = Stringify String

instance Metric Stringify where
  order = Const ["display"]
  fields (Stringify a) =
    ["display" C..= a ]
  displayMetric (Stringify a) =
    Builder.fromString a

--   initialMetric = Display ""

-- displayFromString :: [b] -> Display b [Int]
-- displayFromString chs =
--   let
--     v = V.fromList chs
--     idxs = V.toList . V.map fst . V.indexed $ v
--   in
--   Display v idxs

-- getIntList :: Display b [Int] -> [Int]
-- getIntList (Display _ a) = a

-- compactString :: Display b [Int] -> [b]
-- compactString (Display v a) =
--   catMaybes . map (v V.!?) . L.sort $ a


-- data Listed a = Listed { getList :: [Bool], items :: a }
--   deriving (Functor, Show)

-- instance Metric (Listed [Char]) where
--   order = Const ["count", "listed"]
--   fields (Listed bs items) =
--     ["count" C..= (length $ filter id bs)
--     ,"listed" C..= zipWith (\b x -> if b then x else '·') bs items
--     ]
