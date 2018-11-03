{-|
Module      : Control.Reduce.Util
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module provides utils, so that it is easier to write command-line reducers.
-}
module Control.Reduce.Util
  ( -- reduce

  fromCommand
  -- | Re-export Control.Reduce for convenience:
  , module Control.Reduce
  ) where

-- typed-process
import System.Process.Typed

-- filepath
import System.FilePath

-- directory
import System.Directory

-- bytestring
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

-- cryptohash-sha256
import Crypto.Hash.SHA256

-- reduce
import Control.Reduce

-- base
import System.Exit



-- | A filepath predicate is a predicate, that is based on a filepath.
type FilePathPredicate = Predicate FilePath IO

data Test
  = Status ExitCode
  | StreamEq BS.ByteString

-- | We can create a 'FilePathPredicate' from a command.
fromCommand :: FilePath -> [String] -> FilePathPredicate
fromCommand cmd args filepath = do
  (ec, _, _ ) <- readProcess ( proc cmd ( args ++ [filepath] ) )
  return (ec == ExitSuccess)


-- reduce :: a -> IO (Maybe a)
-- reduce =







-- -- | The options to build a command-line reducer.
-- data ReducerOptions a = ReducerOptions
--   {
--     -- | The underlying reducer to be used to reduce the item.
--     reducer :: Reducer a IO
--   ,

--   -- , keepIterations :: !Bool
--   -- -- ^ Keep the intermediate folders around
--   -- , command :: !FilePath
--   -- -- ^ The command
--   -- , args :: ![String]
--   -- -- ^ Arguments to the path

--   --   workFolder :: !FilePath
--   -- -- ^ The folder to run in
--   -- , toFile :: a -> IO FilePath
--   -- -- ^ Convert the `a` to an argument
--   }



-- -- | Runs the reducer is the IO monad
-- reduce :: ReducerOptions a -> a -> IO (Maybe a)
-- reduce ReducerOptions {..} a = do
--   absoluteCommand <- canonicalizePath command
--   ref <- newIORef (0 :: Int)
--   reducer ( predicate absoluteCommand ref ) a
--   where
--     predicate absoluteCommand ref simpl = do
--       i <- atomicModifyIORef' ref (\i -> ( succ i, i))

--       let folder = workFolder </> printf "%04o" i

--       createDirectoryIfMissing True folder

--       success <- withCurrentDirectory folder $ do
--         file <- toFile simpl
--         (ec, _, _ ) <- readProcess ( proc absoluteCommand ( args ++ [file] ) )
--         return (ec == ExitSuccess)

--       when
--         ( not $ keepIterations )
--         ( removePathForcibly folder )

--       return success


-- -- | An optparser
-- reducerOptionsOpt ::
--   FilePath
--   -> Reducer [a] IO
--   -> Parser (([a] -> IO FilePath) -> ReducerOptions [a])
-- reducerOptionsOpt workFolder reducer =
--   ReducerOptions
--   <$> ( strOption
--         ( long "work-folder" <> short 'w' <> value workFolder
--           <> showDefault <> help "The folder to work in folder"
--         ))
--   <*> ( ( flag' ddmin (long "ddmin" <> help "Use ddmin")
--         <|> flag' binaryReduction (long "bired" <> help "Use binary reduction")
--         <|> flag' linaryReduction (long "lired" <> help "Use linary reduction")
--         ) <|> pure reducer)
--   <*> ( switch (long "keep-iterations" <> short 'k') )
--   <*> strArgument ( metavar "CMD" <> help "The predicate script to reduce with" )
--   <*> many ( strArgument ( metavar "ARGS.." <> help "Optional arguments to the predicate script" ))
