--
-- INFOB3CC Concurrency
-- Practical 1: IBAN calculator
-- hallo
-- http://ics.uu.nl/docs/vakken/b3cc/assessment.html
--
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module IBAN (

  Mode(..), Config(..),
  count, list, search

) where

import Control.Concurrent
--import Crypto.Hash.SHA1
--import Data.Atomics                                       ( readForCAS, casIORef, peekTicket )
import Data.IORef
import Data.List                                          ( elemIndex )
import Data.Word
import Data.Maybe                                         ( fromJust )
import System.Environment
import System.IO
import Data.ByteString.Char8                              ( ByteString )
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Char8                    as B8
import Text.Read.Lex (numberToFixed)
import GHC.Generics (DecidedStrictness(DecidedLazy))


-- -----------------------------------------------------------------------------
-- 0. m-test
-- -----------------------------------------------------------------------------

-- Perform the m-test on 'number'
mtest :: Int -> Int -> Bool
mtest m number = value `mod` m == 0
  -- Implement the m-test here!
  where
    (x:xs) = digits number
    value = x * (-1) + sum (zipWith (*) xs [2 ..])

digits :: (Num int, Ord int, Integral int) => int -> [int]
digits number
  |number < 10 = [number]
  |otherwise   = (number `mod` 10) : digits (number `div` 10)


-- -----------------------------------------------------------------------------
-- 1. Counting mode (3pt)
-- -----------------------------------------------------------------------------

count :: Config -> IO Int
count (Config l u m t) = do
  -- Implement count mode here!
  nr <- newIORef l
  writeIORef nr 0
  
  forkIO (do
    temp <- readIORef nr
    --add one
    writeIORef nr 1
    )

  result <- readIORef nr
  readIORef nr


-- -----------------------------------------------------------------------------
-- 2. List mode (3pt)
-- -----------------------------------------------------------------------------

list :: Handle -> Config -> IO ()
list handle config = do
  -- Implement list mode here!
  -- Remember to use "hPutStrLn handle" to write your output.
  undefined


-- -----------------------------------------------------------------------------
-- 3. Search mode (4pt)
-- -----------------------------------------------------------------------------

search :: Config -> ByteString -> IO (Maybe Int)
search config query = do
  -- Implement search mode here!
  undefined


-- -----------------------------------------------------------------------------
-- Starting framework
-- -----------------------------------------------------------------------------

data Mode = Count | List | Search ByteString
  deriving Show

data Config = Config
  { cfgLower   :: !Int
  , cfgUpper   :: !Int
  , cfgModulus :: !Int
  , cfgThreads :: !Int
  }
  deriving Show

-- Evaluates a term, before continuing with the next IO operation.
--
evaluate :: a -> IO ()
evaluate x = x `seq` return ()

-- Forks 'n' threads. Waits until those threads have finished. Each thread
-- runs the supplied function given its thread ID in the range [0..n).
--
forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n work = do
  -- Fork the threads and create a list of the MVars which
  -- per thread tell whether the work has finished.
  finishVars <- mapM work' [0 .. n - 1]
  -- Wait on all MVars
  mapM_ takeMVar finishVars
  where
    work' :: Int -> IO (MVar ())
    work' index = do
      var <- newEmptyMVar
      _   <- forkOn index (work index >> putMVar var ())
      return var

-- Checks whether 'value' has the expected hash.
--
--checkHash :: ByteString -> String -> Bool
--checkHash expected value = expected == hash (B8.pack value)
