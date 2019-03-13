{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Reduce.Util.Metric where

-- time
import           Data.Time

-- lens
import           Control.Lens

-- vector
import qualified Data.Vector                      as V

-- text
import qualified Data.Text.Lazy.Builder           as Builder

-- bytestring
import qualified Data.ByteString.Char8            as BS

-- cassava
import qualified Data.Csv                         as C

-- base
import           GHC.Generics                     (Generic)

-- reduce-util
import           Control.Reduce.Util.CliPredicate
import qualified Control.Reduce.Util.Logger       as L

-- * Metrics
class Metric a where
   order :: Const [BS.ByteString] a
   fields :: a -> [(BS.ByteString, BS.ByteString)]
   displayMetric :: a -> Builder.Builder

data MetricRow a = MetricRow
  { metricRowContent :: !a
  , metricRowTime    :: !NominalDiffTime
  , metricRowFolder  :: !String
  , metricRowResults :: !(CmdResult (Maybe CmdOutput))
  , metricRowSuccess :: !Bool
  } deriving (Show, Eq, Generic, Functor)

instance Metric a => C.DefaultOrdered (MetricRow a) where
  headerOrder _ =
    C.header $
    getConst (order :: Const [BS.ByteString] a)
    <> [ "folder"
       , "time"
       , "success"
       , "setup time"
       , "run time"
       , "status"
       , "stdout (length)"
       , "stdout (sha256)"
       , "stderr (length)"
       , "stderr (sha256)"
       ]

instance Metric a => C.ToNamedRecord (MetricRow a) where
  toNamedRecord MetricRow {..} =
    C.namedRecord $
    fields metricRowContent <>
    [ "folder" C..= metricRowFolder
    , "time" C..= (realToFrac metricRowTime :: Double)
    , "setup time" C..= resultSetupTime metricRowResults
    , "run time" C..= resultRunTime metricRowResults
    , "status" C..= maybe (-1) (exitCodeToInt . outputCode) result
    , "stdout (length)"
      C..= maybe (-1 :: Int) (fromIntegral . snd . outputOut) result
    , "stdout (sha256)"
      C..= maybe "-" (showHash . fst . outputOut) result
    , "stderr (length)"
      C..= maybe (-1 :: Int) (fromIntegral . snd . outputErr) result
    , "stderr (sha256)"
      C..= maybe "-" (showHash . fst . outputErr) result
    , "success" C..= (if metricRowSuccess then "true" else "false" :: String)
    ]
    where
      result = resultOutput metricRowResults

counted :: [b] -> Count
counted =
  Count . length

data Count = Count { getCount :: Int }
  deriving (Show)

instance Metric Count where
  order = Const ["count"]
  fields (Count a) =
    ["count" C..= a ]
  displayMetric (Count a) =
    L.displayf "#%i elements" a

instance (Metric a, Metric b) => Metric (a, b) where
  order = Const (getConst (order :: Const [BS.ByteString] a) ++ getConst (order :: Const [BS.ByteString] b))
  fields (a, b) =
    fields a ++ fields b
  displayMetric (a, b) =
    displayMetric a <> " and " <> displayMetric b

-- displayed :: (b -> Char) -> [b] -> Display
-- displayed toChar =
--   let
--   model
--     idxs
--     (ba . catMaybes . map (v V.!?) . L.sort)
--     (displ . fst)

--   where
--     v = V.fromList b
--     str = V.map toChar v
--     idxs = V.toList . V.map fst . V.indexed $ v
--     displ a =
--       let is = IS.fromList a in
--       Display . V.toList $ V.imap (\i c -> if IS.member i is then c else '·') str

newtype Display = Display String

instance Metric Display where
  order = Const ["display"]
  fields (Display a) =
    ["display" C..= a ]
  displayMetric (Display a) =
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
