{-# LANGUAGE ExistentialQuantification, PolymorphicComponents #-}
module Data.Zippable.Definition
	( Zippable(..)
	, CError
	, throwCError
	, cErrorToEither
	) where

import Data.Traversable

-- | Efficient error reporting monad, as described in the paper \"Asymptotic
-- Improvement of Computations over Free Monads\" by Janis Voigtländer.
--
-- Can be used in places where one would use ('Either' String).
newtype CError a = CError (forall b. (a -> Either String b) -> Either String b)

instance Monad CError where
	return a = CError (\h -> h a)
 	(CError p) >>= k = CError (\h -> p (\a -> case k a of CError q -> q h))

-- | Throw an error inside a 'CError' computation. Compare with 'Left' in the
-- ('Either' String) monad.
throwCError :: String -> CError a
throwCError err = CError (const (Left err))

-- | Run a 'CError' computation, and convert any error to 'Left' values.
cErrorToEither :: CError a -> Either String a
cErrorToEither (CError p) = p Right

-- | Data structures that can be folded.
--
-- Minimal complete definition: any of 'tryZipWith'' (preferred for efficiency), 'tryZipWith', or 'tryZip'
--
-- For example, given a data type:
--
-- > data Tree a = Leaf a | Node (Tree a) (Tree a)
-- 
-- a suitable instance would be
-- 
-- > instance Zippable Tree where
-- >   tryZipWith' func (Leaf a)     (Leaf b)     = Right (Leaf (func a b))
-- >   tryZipWith' func (Node a1 a2) (Node b1 b2) = do z1 <- tryZipWith' func a1 b1
-- >                                                   z2 <- tryZipWith' func a2 b2
-- >                                                   return (Node z1 z2)
-- >   tryZipWith' _ _ _ = throwCError "Shape mismatch."
class Traversable k => Zippable k where

  -- | Zip the elements of two structures with the given CError computation.
  --
  -- @'tryZipWith'' func x y = either 'throwCError' (Data.Traversable.mapM (uncurry func)) ('tryZip' x y)
  tryZipWith' ::(a -> b -> CError c) ->  k a -> k b -> CError (k c)
  tryZipWith' func x y = either throwCError (Data.Traversable.mapM (uncurry func)) (tryZip x y)

  -- | Zip the elements of two structures with the given function.
  -- 
  -- @'tryZipWith' func x y = 'cErrorToEither' ('tryZipWith'' (\a b -> return (func a b)) x y)@
  tryZipWith :: (a -> b -> c) ->  k a -> k b -> Either String (k c)
  tryZipWith func x y = cErrorToEither (tryZipWith' (\a b -> return (func a b)) x y)

  -- | Zip the elements of two structures as tuples.
  --
  -- @'tryZip' = 'tryZipWith' (,)@
  tryZip :: k a -> k b -> Either String (k (a,b))
  tryZip = tryZipWith (,)

