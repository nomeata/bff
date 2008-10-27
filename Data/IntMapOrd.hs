-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMapOrd
-- 
-- Maintainer  :  Janis Voigtlaender
-- Stability   :  experimental
--
-- A variant of the regular 'Data.IntMap', enforcing that the map is monotonous
-- with regard to 'Ord'.
--
-----------------------------------------------------------------------------
module Data.IntMapOrd 
  ( IntMapOrd,
    empty,
    checkInsert,
    lookup,
    lookupR,
    member,
    memberR,
    union,
    fromAscPairList,
    toList ) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Prelude hiding (lookup)

newtype IntMapOrd a = IntMapOrd (Bimap.Bimap Int a) 

instance Show a => Show (IntMapOrd a) where
  show (IntMapOrd m) = show m

-- | /O(log n)/. Lookup the key for a value in the map.
lookupR :: Ord a => a -> IntMapOrd a -> Maybe Int
lookupR a (IntMapOrd m) = Bimap.lookupR a m

-- | /O(log n)/. Is the key a member of the map?
member :: Ord a => Int -> IntMapOrd a -> Bool
member k (IntMapOrd m) = Bimap.member k m

-- | /O(log n)/. Is the value a member of the map?
memberR :: Ord a => a -> IntMapOrd a -> Bool
memberR a (IntMapOrd m) = Bimap.memberR a m

-- | /O(n)/.  Build a map from a list of pairs, where
-- both the fst and snd parts of the list are in strictly ascending order.
-- 
-- This precondition is not checked; an invalid list will produce a malformed map. 
fromAscPairList :: Ord a => [(Int,a)] -> IntMapOrd a
fromAscPairList l = IntMapOrd (Bimap.fromAscPairListUnchecked l)

-- | /O(1)/. The empty map. 
empty :: IntMapOrd a
empty = IntMapOrd Bimap.empty

-- | /O(log n)/. Lookup the value at a key in the map.
lookup :: Ord a => Int -> IntMapOrd a -> Maybe a
lookup k (IntMapOrd m) = Bimap.lookup k m

insert :: Ord a => Int -> a -> IntMapOrd a -> Either String (IntMapOrd a)
insert k a (IntMapOrd m) = let (m1,m2) = Map.split k (Bimap.toMap m)
                           in  if (Map.null m1 || snd (Map.findMax m1) < a) 
                                  && (Map.null m2 || snd (Map.findMin m2) > a) 
                               then Right (IntMapOrd (Bimap.insert k a m)) 
                               else Left "Update violates relative order."

-- |  /O(log n)/. Insert a new key\/value pair in the map. Errors are:
--
--  * Inserting an existing key with a different value.
--
--  * Inserting a key with a value that already exists for another key.
--
--  * Inserting a key\/value pair that breaks the monotonicity invariant.
checkInsert :: Ord a => Int -> a -> IntMapOrd a -> Either String (IntMapOrd a)
checkInsert i b m = case lookup i m of
                      Nothing -> if memberR b m
                                   then Left "Update violates unequality."
                                   else insert i b m
                      Just c  -> if b==c 
                                   then Right m 
                                   else Left "Update violates equality."

-- | /O(n * log n)/. The union of two maps. It prefers the first map
-- when duplicate keys are encountered. If the monotonicity invariant is violated,
-- an error is signalled with a 'Left' return value.
union :: Ord a => IntMapOrd a -> IntMapOrd a -> Either String (IntMapOrd a)
union h (IntMapOrd m) = Bimap.fold f (Right h) m
  where f k a (Right h) = if member k h 
                            then Right h 
                            else if memberR a h
                                   then Left "Update violates unequality."
                                   else insert k a h
        f k a l         = l

-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: IntMapOrd a -> [(Int,a)]
toList (IntMapOrd a) = Bimap.toList a
