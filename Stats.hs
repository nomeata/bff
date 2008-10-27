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
import Data.Traversable hiding (mapM)
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
import Graphics.Rendering.Chart
import System.IO
import List

-------------------
-- Configuration --
-------------------

sizes = (\n -> [0,(n`div`100)..n]) 100000
repetitions = 3
tests_to_run = do
          runTest test1
	  runTest test2
	  runTest test3
	  runTest test4
	  runTest test5

----------------------
-- Test definitions --
----------------------

data Test c c' a = Test
	{ testName    :: String
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
	{ testName    = "halve, scaled"
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
	{ testName    = "flatten, left-leaning"
	, genTestCase = fix (\loop n -> if n == 1
                                        then Leaf ()
                                        else Node (loop (n-1)) (Leaf ()))
	, getTest     = flatten
	, putTestMan  = put2
	, putTestAuto = bff flatten
	, modifyTest  = id
        , scale       = const
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
	{ testName    = "flatten, right-leaning"
	, genTestCase = fix (\loop n -> if n == 1
                                        then Leaf ()
                                        else Node (Leaf ()) (loop (n-1)))
	, getTest     = flatten
	, putTestMan  = put2
	, putTestAuto = bff flatten
	, modifyTest  = id
        , scale       = const
	}

--
-- Test 4
--

test4 = Test
	{ testName    = "nodups, all unequal"
	, genTestCase = \n -> [1..n]
	, getTest     = nodups
	, putTestMan  = put3
	, putTestAuto = bff_Eq nodups
	, modifyTest  = id
        , scale       = const
	}

nodups :: Eq a => [a] -> [a]
nodups = List.nub

put3 :: Eq a => [a] -> [a] -> [a]
put3 s v | v == List.nub v && length v == length s'
         = map (fromJust . flip lookup (zip s' v)) s
           where s' = List.nub s

--
-- Test 5
--

test5 = Test
	{ testName    = "nodups, all equal"
	, genTestCase = \n -> map (const 0) [1..n]
	, getTest     = nodups
	, putTestMan  = put3
	, putTestAuto = bff_Eq nodups
	, modifyTest  = id
        , scale       = const
	}

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

collectStats test = mapM (\size -> do
		putStr "."
		manual <- stats test putTestMan size
		putStr "."
	        automatic <- stats test putTestAuto size
		putStr " "
                return (size, manual, automatic)
	) sizes

putTable statData = putStr (tabelize stringData)
  where stringData = ["Size","Manual Getter", "Automatic Getter"] :
                     map (\(s,m,a) -> [show s, printf "%.3f" m, printf "%.3f" a]) statData

tabelize :: [[String]] -> String
tabelize table = unlines (map padLine table)
  where colWidths = map (maximum . map length) (transpose table)
        pad w s = replicate (w - length s) ' ' ++ s
	padLine ss = intercalate "|" (zipWith pad colWidths ss)

putGraph test statData filename = renderableToPDFFile r 800 400 filename
  where r = toRenderable $
		defaultLayout1
		{ layout1_title = "Test \"" ++ testName test ++"\""
		, layout1_plots = [
			("Manual Getter", HA_Bottom, VA_Left,
				toPlot $ defaultPlotLines 
					{ plot_lines_values = [manualData]
					, plot_lines_style = solidLine 1 (Color 0 0 1)
					}
			),
			("Automatic Getter", HA_Bottom, VA_Left,
				toPlot $ defaultPlotLines 
					{ plot_lines_values = [automaticData]
					, plot_lines_style = solidLine 1 (Color 1 0 0)
					}
			)
			]
		, layout1_vertical_axes = 
			linkedAxes' (autoScaledAxis (defaultAxis {axis_title =
					"Average time per test in ms"
				}))
		, layout1_horizontal_axes = 
			linkedAxes' (autoScaledAxis (defaultAxis {axis_title =
					"Size of input data structure"
				}))
		}
        (manualData, automaticData) = unzip $ map f statData
	f (s, m, a) = (Point (fromIntegral s) (scale test m s), Point (fromIntegral s) (scale test a s))

runTest :: (Show (c v), Show (c' v), F.Foldable c', Zippable c', Traversable c, Eq v) =>
           Test c c' v -> IO ()
runTest test = do
	  putStrLn $ "" 
	  putStr   $ "Test \"" ++ testName test ++ "\" "
	  statData <- collectStats test
          putStrLn $ ""
	  putTable statData
	  let graphFileName = testName test ++ ".pdf"
	  putStrLn $ "Writing graph to " ++ graphFileName
	  putGraph test statData graphFileName

main = do hSetBuffering stdout NoBuffering
	  putStrLn $ "Bff benchmarking program"
          putStrLn $ "(c) 2008 Joachim Breitner"
	  putStrLn $ "Repeating every test " ++ show repetitions ++ " times."
	  tests_to_run

-- Data Definition
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
$(derive makeZippable ''Tree)
-- Lined in, otherwise name clashes with Prelude.foldr occurs
$(derive makeFunctor ''Tree)
instance F.Foldable Tree where
      foldr f b (Leaf a1) = (f a1 . id) b
      foldr f b (Node a1 a2) = (flip (F.foldr f) a1 . (flip (F.foldr f) a2 . id)) b 
$(derive makeTraversable ''Tree)
