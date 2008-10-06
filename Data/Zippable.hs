module Data.Zippable where

class Zippable k where
  tryZip :: k Int -> k a -> Either String (k (Int,a))

