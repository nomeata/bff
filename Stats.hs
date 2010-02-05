{-# LANGUAGE ExistentialQuantification, PolymorphicComponents,
             TemplateHaskell
  #-}

import Test.BenchPress
import Control.Exception (evaluate)
import Data.Bff
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Fix
import Data.Traversable hiding (mapM, sequence)
import qualified Data.Traversable as T
import qualified Data.Foldable as F 
import Data.Zippable
import Data.DeriveTH
import Data.Derive.Zippable
import Data.Derive.Traversable
import Data.Derive.Functor
import Data.List
import Prelude
import Data.Ord
import Data.Maybe
import Text.Printf
import System.IO
import List
import StatsDef

-------------------
-- Configuration --
-------------------

sizes = (\n -> [1,(n`div`200)..n])
repetitions = 10
tests_to_run =
	[ runTest test1
        , runTest test2
        , runTest test3
        , runTest test4
	]

----------------------
-- Test definitions --
----------------------

data Test c c' a = Test
	{ testName    :: String
        , maxN        :: Int
	, genTestCase :: Int  -> c  a
        , getTest     :: c a  -> c' a 
	, putTestMan  :: c a  -> c' a -> c a
	, putTestAuto :: c a  -> c' a -> c a
        , modifyTest  :: c' a -> c' a
        , scale       :: Double -> Int -> Double
	}

--
-- Test 1
--

test1 = Test
	{ testName    = "halve, normalized"
	, maxN        = 100000
	, genTestCase = \n -> [1..n]
	, getTest     = halve 
	, putTestMan  = put1
	, putTestAuto = bff halve
	, modifyTest  = id
        , scale       = \m s -> m / fromIntegral s
	}

halve :: [a] -> [a]
halve as = take (length as `div` 2) as

put1 :: [a] -> [a] -> [a]
put1 as as' | length as' == n
            = as' ++ drop n as
              where n = length as `div` 2 

--
-- Test 2
--

test2 = Test
	{ testName    = "flatten, left-leaning trees, normalized"
	, maxN        = 5000
	, genTestCase = fix (\loop n -> if n == 1
                                        then Leaf ()
                                        else Node (loop (n-1)) (Leaf ()))
	, getTest     = flatten
	, putTestMan  = put2
	, putTestAuto = bff flatten
	, modifyTest  = id
        , scale       = \m s -> m / fromIntegral s
	}

flatten :: Tree a -> [a]
flatten (Leaf a)     = [a]
flatten (Node t1 t2) = flatten t1 ++ flatten t2

put2 :: Tree a -> [a] -> Tree a
put2 s v = case go s v of (t,[]) -> t
  where go (Leaf a) (b:bs) = (Leaf b,bs)
        go (Node s1 s2) bs = (Node t1 t2,ds)
          where (t1,cs) = go s1 bs
                (t2,ds) = go s2 cs

--
-- Test 3
--

test3 = Test
	{ testName    = "flatten, right-leaning trees, normalized"
	, maxN        = 100000
	, genTestCase = fix (\loop n -> if n == 1
                                        then Leaf ()
                                        else Node (Leaf ()) (loop (n-1)))
	, getTest     = flatten
	, putTestMan  = put2
	, putTestAuto = bff flatten
	, modifyTest  = id
        , scale       = \m s -> m / fromIntegral s
	}

--
-- Test 4
--

test4 = Test
	{ testName    = "rmdups, all elements different, normalized"
	, maxN        = 10000
	, genTestCase = \n -> [1..n]
	, getTest     = rmdups
	, putTestMan  = put3
	, putTestAuto = bff_Eq rmdups
	, modifyTest  = id
        , scale       = \m s -> m / fromIntegral s
	}

rmdups :: Eq a => [a] -> [a]
rmdups = List.nub

put3 :: Eq a => [a] -> [a] -> [a]
put3 s v | v == List.nub v && length v == length s'
         = map (fromJust . flip lookup (zip s' v)) s
           where s' = List.nub s

----------------------------------
-- Stats calculation and output --
----------------------------------

stats test putter size = (mean . fst) `fmap` benchmark repetitions
		(do let source = genTestCase test size
		        view   = getTest test source
                        view'  = modifyTest test view
                    deepEvaluate (source, view')
                )
		(\_ -> return ())
		(\(source, view') -> deepEvaluate (putter test source view')
		)

deepEvaluate :: Show a => a -> IO a
deepEvaluate x = evaluate (length (show x)) >> return x

collectStats :: (Show (c a), Show (c' a)) => Test c c' a -> IO StatRunData
collectStats test = mapM (\size -> do
		putStr "."
		manual <- stats test putTestMan size
		putStr "."
	        automatic <- stats test putTestAuto size
		putStr " "
                return (size, scale test manual size, scale test automatic size)
	) (sizes (maxN test))

runTest :: (Show (c v), Show (c' v), F.Foldable c', Zippable c', Traversable c, Eq v) =>
           Test c c' v -> IO (String, StatRunData)
runTest test = do
	  putStrLn $ "" 
	  putStr   $ "Test \"" ++ testName test ++ "\" "
	  statData <- collectStats test
          putStrLn $ ""
	  return (testName test, statData)

main = do hSetBuffering stdout NoBuffering
	  putStrLn $ "Bff benchmarking program"
          putStrLn $ "(c) 2008 Joachim Breitner"
	  putStrLn $ "Repeating every test " ++ show repetitions ++ " times."
	  rawData <- sequence tests_to_run
	  putStrLn $ "Writing data to stats.data"
	  writeFile "stats.data" (show rawData)

-- Data Definition
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
-- Lined in, otherwise name clashes with Prelude.foldr occurs
$(derive makeFunctor ''Tree)
instance F.Foldable Tree where
      foldr f b (Leaf a1) = (f a1 . id) b
      foldr f b (Node a1 a2) = (flip (F.foldr f) a1 . (flip (F.foldr f) a2 . id)) b 
$(derive makeTraversable ''Tree)
$(derive makeZippable ''Tree)
