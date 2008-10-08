{-# OPTIONS_GHC  -XTemplateHaskell -XFlexibleInstances -XFlexibleContexts -fallow-undecidable-instances  #-}

module TestZippableDeriver where

import Data.Zippable
import Data.DeriveTH
import Data.Derive.Zippable

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
data Tree2 a = Leaf2 (Maybe a) | Node2 (Tree2 a) (Tree a) deriving Show
data Tree3 a = Leaf3 (Maybe (Int,Int)) a | Node3 (Tree3 a) (Tree3 a) deriving Show
data Pair a = Pair (Int,a,(Int,a))
data Nested a = Nested [Maybe (Maybe (Either Int Int,a))]
data MaybesTuples a = MaybesTuples ( Maybe [ Tree [ Maybe [ Maybe [Maybe [
	(a, Int, a, Int, a, Int, Tree a)
	]]]]])

{-
instance Zippable [] where
  tryZip []     []     = Right []
  tryZip (i:is) (b:bs) = Right (:) <*> Right (i,b)
                                   <*> tryZip is bs
  tryZip _      _      = Left "Update changes the length."


instance Zippable Tree where
  tryZip (Leaf i)     (Leaf b)     = Right (Leaf (i,b))
  tryZip (Node t1 t2) (Node v1 v2) = Right Node 
                                     <*> tryZip t1 v1 
                                     <*> tryZip t2 v2
  tryZip _ _ = Left "Update changes the shape."
-}

test1 = (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
test2 = (Node (Leaf "eins") (Node (Leaf "zwei") (Leaf "drei")))
test3 = (Node (Node (Leaf "01") (Leaf "10")) (Leaf "11"))
test4 = (Node (Leaf "one") (Leaf "two"))

{-
-- Not good, does not warn on structural change:

instance (Traversable t) => Zippable t where
	tryZip c1 c2 = let list1 = execWriter $ T.mapM (tell . (:[])) c1 
			   list2 = execWriter $ T.mapM (tell . (:[])) c2
			   ret = flip evalState (zip list1 list2) $
				    T.mapM (\_ -> do new <- gets head
						     modify tail
						     return new) c1
		       in  if length list1 == length list2
		           then Right ret
                           else Left "Differnt number of values contained in argument to tryZip."
-}

$(derive makeZippable ''Tree)
$(derive makeZippable ''Tree2)
$(derive makeZippable ''Tree3)
$(derive makeZippable ''Pair)
$(derive makeZippable ''Nested)
$(derive makeZippable ''MaybesTuples)

