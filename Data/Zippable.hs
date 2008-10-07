module Data.Zippable where

class Zippable k where
  tryZipWith :: (a -> b -> Either String c) ->  k a -> k b -> Either String (k c)
  tryZip :: k a -> k b -> Either String (k (a,b))
  tryZip = tryZipWith (\a b -> Right (a,b))

