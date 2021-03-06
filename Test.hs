{-# OPTIONS_GHC  -XTemplateHaskell -XFlexibleInstances -XFlexibleContexts -fallow-undecidable-instances  #-}

module Test where

import Control.Applicative 
import Control.Functor.Combinators.Lift
import Control.Monad.Either
import Control.Monad.Writer
import Control.Monad.State
import Data.Traversable
import qualified Data.Traversable as T
import Data.Foldable
import Data.Zippable
import Data.DeriveTH
import Data.Derive.Traversable

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

$( derive makeTraversable ''Tree )

instance Foldable Tree where
  foldMap = foldMapDefault

instance Functor Tree where
  fmap = fmapDefault

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


instance (Traversable k1, Traversable k2) 
         => Traversable (Lift (,) k1 k2) where
  traverse f (Lift (k1,k2)) = pure (curry Lift) 
                              <*> traverse f k1
                              <*> traverse f k2

instance (Traversable k1, Traversable k2) 
         => Foldable (Lift (,) k1 k2) where
  foldMap = foldMapDefault

instance (Zippable k1, Zippable k2) 
         => Zippable (Lift (,) k1 k2) where
  tryZip (Lift (k1,k2)) (Lift (k1',k2')) = Right (curry Lift) 
                                           <*> tryZip k1 k1' 
                                           <*> tryZip k2 k2'

instance (Show (k1 a), Show (k2 a)) 
         => Show (Lift (,) k1 k2 a) where
  show (Lift p) = "Lift " ++ show p

test1 = (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
test2 = (Node (Leaf "eins") (Node (Leaf "zwei") (Leaf "drei")))
test3 = (Node (Node (Leaf "01") (Leaf "10")) (Leaf "11"))
test4 = (Node (Leaf "one") (Leaf "two"))


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
