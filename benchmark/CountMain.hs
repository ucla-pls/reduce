
import Control.Reduce
import Control.Monad.Writer

import Data.Monoid

import qualified Data.List as L

main = do
  runWithAll "one" all [0..99] [[54]]
  runWithAll "two" all [0..99] [[23, 74]]
  runWithAll "three" all [0..99] [[23, 50, 74]]
  runWithAll "contiguous-small" all [0..99] [[15..25]]
  runWithAll "contiguous-large" all [0..99] [[15..75]]
  runWithAll "spread-large" all [0..99] [[0,2..99]]
  runWithAll "all" all [0..99] [[0..99]]
  where
    all = [ ("ddmin", ddmin)
          , ("bred", binaryReduction)
          , ("rbred", revBinaryReduction)]

runWithAll ::
  String
  -> [(String, TestReducer)]
  -> [Int]
  -> [[Int]]
  -> IO ()
runWithAll casename ps xs minima =
  forM_ ps $ \(name, red) -> do
    let r = runCase red xs minima
    putStrLn $ L.intercalate ","
      [ casename, name, show (L.length (resTries r)), show (resMinimal r)]


type TestReducer = Reducer [Int] ((,) (Endo [(Bool,Int)]))

data Result = Result
  { resTries :: [(Bool, Int)]
  , resMinimal :: Bool
  } deriving (Show)

runCase ::
  TestReducer
  -> [Int]
  -> [[Int]]
  -> Result
runCase red space minima =
  let
    (m, res) = red pred space
  in
    Result
    { resTries = appEndo m []
    , resMinimal =
      case res of
        Just x -> any (x ==) minima
        Nothing -> False
    }
  where
    pred :: forall m. MonadWriter (Endo [(Bool, Int)]) m => [Int] -> m Bool
    pred xs = do
      let t = hasMinima xs
      tell (Endo ((t, L.length xs):))
      return t
    hasMinima xs = any (flip L.isSubsequenceOf xs) minima
