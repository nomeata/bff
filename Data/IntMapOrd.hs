module Data.IntMapOrd 
  ( IntMapOrd,
    lookupR,
    lookup,
    union,
    empty,
    checkInsert,
    fromAscPairList ) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Prelude hiding (lookup)

newtype IntMapOrd a = IntMapOrd (Bimap.Bimap Int a) 

instance Show a => Show (IntMapOrd a) where
  show (IntMapOrd m) = show m

lookupR :: Ord a => a -> IntMapOrd a -> Maybe Int
lookupR a (IntMapOrd m) = Bimap.lookupR a m

member :: Ord a => Int -> IntMapOrd a -> Bool
member k (IntMapOrd m) = Bimap.member k m

memberR :: Ord a => a -> IntMapOrd a -> Bool
memberR a (IntMapOrd m) = Bimap.memberR a m

fromAscPairList :: Ord a => [(Int,a)] -> IntMapOrd a
fromAscPairList l = IntMapOrd (Bimap.fromAscPairListUnchecked l)

empty :: IntMapOrd a
empty = IntMapOrd Bimap.empty

lookup :: Ord a => Int -> IntMapOrd a -> Maybe a
lookup k (IntMapOrd m) = Bimap.lookup k m

insert :: Ord a => Int -> a -> IntMapOrd a -> Either String (IntMapOrd a)
insert k a (IntMapOrd m) = let (m1,m2) = Map.split k (Bimap.toMap m)
                           in  if (Map.null m1 || snd (Map.findMax m1) < a) 
                                  && (Map.null m2 || snd (Map.findMin m2) > a) 
                               then Right (IntMapOrd (Bimap.insert k a m)) 
                               else Left "Update violates relative order."

checkInsert :: Ord a => Int -> a -> IntMapOrd a -> Either String (IntMapOrd a)
checkInsert i b m = case lookup i m of
                      Nothing -> if memberR b m
                                   then Left "Update violates unequality."
                                   else insert i b m
                      Just c  -> if b==c 
                                   then Right m 
                                   else Left "Update violates equality."

union :: Ord a => IntMapOrd a -> IntMapOrd a -> Either String (IntMapOrd a)
union h (IntMapOrd m) = Bimap.fold f (Right h) m
  where f k a (Right h) = if member k h 
                            then Right h 
                            else if memberR a h
                                   then Left "Update violates unequality."
                                   else insert k a h
        f k a l         = l
