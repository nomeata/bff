{-# LANGUAGE ExistentialQuantification, PolymorphicComponents #-}
module Data.Zippable 
	( CError
	, throwCError
	, cErrorToEither
	, Zippable(..)
	) where

newtype CError a = CError (forall b. (a -> Either String b) -> Either String b)

instance Monad CError where
	return a = CError (\h -> h a)
 	(CError p) >>= k = CError (\h -> p (\a -> case k a of CError q -> q h))

throwCError :: String -> CError a
throwCError err = CError (const (Left err))

cErrorToEither :: CError a -> Either String a
cErrorToEither (CError p) = p Right

class Zippable k where
  tryZipWith' :: (a -> b -> CError c) ->  k a -> k b -> CError (k c)
  tryZipWith :: (a -> b -> c) ->  k a -> k b -> Either String (k c)
  tryZipWith combiner  x y = cErrorToEither (tryZipWith' (\x y -> return (combiner x y)) x y)
  tryZip :: k a -> k b -> Either String (k (a,b))
  tryZip = tryZipWith (,)

