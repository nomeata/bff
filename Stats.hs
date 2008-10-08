{-# LANGUAGE ExistentialQuantification, PolymorphicComponents,
             TemplateHaskell
  #-}

import Test.BenchPress
import Control.Exception (evaluate)
import Bff
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

-- Configuration
sizes = [0,10^3..10^4]
repetitions = 1

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
$(derive makeZippable ''Tree)
-- Lined, otherwise name clashes occur
$(derive makeFunctor ''Tree)
instance F.Foldable Tree where
      foldr f b (Leaf a1) = (f a1 . id) b
      foldr f b (Node a1 a2) = (flip (F.foldr f) a1 . (flip (F.foldr f) a2 . id)) b 
$(derive makeTraversable ''Tree)


data Test c c' a = Test
	{ testName    :: String
	, genTestCase :: Int  -> c  a
        , getTest     :: c a  -> c' a -- Polymorphic type important!
	, putTestMan  :: c a  -> c' a -> c a
	, putTestAuto :: c a  -> c' a -> c a
        , modifyTest  :: c' a -> c' a
	}

test1 = Test
	{ testName    = "Halve"
	, genTestCase = \n -> [1..n]
	, getTest     = get 
	, putTestMan  = \as as' -> as' ++ drop (length as `div` 2) as
	, putTestAuto = bff get
	, modifyTest  = pairDance
	}
  where get :: [a] -> [a]
        get as = take (length as `div` 2) as

test2 :: Test Tree [] ()
test2 = Test
	{ testName   = "Flatten"
	, genTestCase = fix (\loop n -> if n <= 1
                                        then Leaf ()
                                        else Node (loop (n`div`2)) (loop (n - (n`div`2))))
	, getTest     = get
	, putTestMan  = \s v -> flip evalState v $
				    T.mapM (\_ -> do new <- gets head
						     modify tail
						     return new) s
	, putTestAuto = bff get
	, modifyTest  = pairDance
	}
  where get :: Tree a -> [a]
 	get = execWriter . T.mapM (tell . (:[])) 

test3 :: Test [] [] Int
test3 = Test
	{ testName   = "nub"
	, genTestCase = fix (\loop n -> if n <= 1
                                        then [] else
                                        [1..n`div`2] ++ loop (n - (n`div`2)))
	, getTest     = get
	, putTestMan  = \s v -> let mapping = zip (get s) v
                                in  map (\x -> fromJust (lookup x mapping)) s
	, putTestAuto = bff_Eq get
	, modifyTest  = pairDance
	}
  where get :: Eq a => [a] -> [a]
 	get = nub

pairDance [] = []
pairDance [x] = [x]
pairDance (a:b:r) = (b:a:pairDance r)

stats test putter size = (mean . fst) `fmap` benchmark repetitions
		(do let source = genTestCase test size
		        view   = getTest test source
                        view'   = modifyTest test view
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

putGraph test statData filename = renderableToPNGFile r 800 400 filename
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
	f (s, m, a) = (Point (fromIntegral s) m, Point (fromIntegral s) a)

runTest :: (Show (c v), Show (c' v), F.Foldable c', Zippable c', Traversable c, Eq v) =>
           Test c c' v -> IO ()
runTest test = do
	  putStrLn $ "" 
	  putStr   $ "Test \"" ++ testName test ++ "\" "
	  statData <- collectStats test
          putStrLn $ ""
	  putTable statData
	  let graphFileName = testName test ++ ".png"
	  putStrLn $ "Writing graph to " ++ graphFileName
	  putGraph test statData graphFileName

main = do hSetBuffering stdout NoBuffering
	  putStrLn $ "Bff benchmarking program"
          putStrLn $ "(c) 2008 Joachim Breitner"
	  putStrLn $ "Repeating every test " ++ show repetitions ++ " times."
          runTest test1
	  runTest test2
	  runTest test3
