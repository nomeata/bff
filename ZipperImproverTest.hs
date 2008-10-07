{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- import Data.Zippable
import Control.Monad.Either
import Improve
import System.CPUTime

-- Normal version, as derived (with (,) inlined into tryZipWith)

tryZip :: Tree a -> Tree b -> Either String (Tree (a,b))
tryZip (Leaf x1) (Leaf y1)
	   = do return (Leaf (x1,y1))
tryZip (Node x1 x2) (Node y1 y2)
	   = do z1 <- tryZip x1 y1
		z2 <- tryZip x2 y2
		return (Node z1 z2)
tryZip _ _ = Left "Structure mismatch in tryZip" 

-- Functor whose Free Monad is isomorphic to Either String

data F_Either b = F_Error String deriving Show

instance Functor F_Either where
	fmap _ (F_Error e) = (F_Error e)

-- The equivalent to Left

left s = wrap (F_Error s)

tryZip' :: FreeLike F_Either m => Tree a -> Tree b -> m (Tree (a,b))
tryZip' (Leaf x1) (Leaf y1)
	   = do return (Leaf (x1,y1))
tryZip' (Node x1 x2) (Node y1 y2)
	   = do z1 <- tryZip' x1 y1
		z2 <- tryZip' x2 y2
		return (Node z1 z2)
tryZip' _ _ = left "Structure mismatch in tryZip" 


-- Getting a proper Either value back in the end

toEither (Return v) = Right v
toEither (Wrap (F_Error s)) = Left s

-- Generate a large Tree. Large enough so that +RTS -K100M is reqiured for all
-- but the optimized runs below
genLargeTree left variant = go 2000000
  where go 0 | odd variant = Leaf variant
             | otherwise   = Node (Leaf variant) (Leaf variant)
        go n = if left then Node (go (n-1)) (Leaf 0)
                       else Node (Leaf 0)   (go (n-1))


-- Benchmarking for the poor
time action = do before <- getCPUTime
                 action
                 after <- getCPUTime
		 putStrLn $ (show (after - before)) ++ " ps."

main = do putStrLn "With Either monad:"
          let largeTree1 = genLargeTree True 1
              largeTree2 = genLargeTree True 2
	  time $ result (tryZip largeTree1 largeTree2)

	  putStrLn "With Free Monad, without improve:"
          let largeTree1 = genLargeTree True 3
              largeTree2 = genLargeTree True 4
	  time $ result (toEither (tryZip' largeTree1 largeTree2))

	  putStrLn "With Free Monad, with improve:"
          let largeTree1 = genLargeTree True 5
              largeTree2 = genLargeTree True 6
	  time $ result (toEither (improve (tryZip' largeTree1 largeTree2)))

 where result = putStrLn . either (const "Done") (const "Bug?") 
	
