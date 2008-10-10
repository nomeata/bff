-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMapEq
-- Copyright   :  (c) 2008 Janis Voigtländer
-- 
-- Maintainer  :  Janis Voigtländer
-- Stability   :  experimental
--
-- A variant of the regular 'Data.IntMap', enforcing that the map is injectiv (up to '==').
--
-- As with 'Data.IntMap', many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of elements with a
-- maximum of W -- the number of bits in an Int  (32 or 64).
-----------------------------------------------------------------------------
module Data.IntMapEq 
  ( IntMapEq,
    empty,
    insert,
    checkInsert,
    lookup,
    lookupR,
    member,
    memberR,
    union,
    toList ) where

import qualified Data.IntMap as IntMap
import Prelude hiding (lookup)
import qualified Prelude

newtype IntMapEq a = IntMapEq (IntMap.IntMap a)

instance Show a => Show (IntMapEq a) where
  show (IntMapEq m) = show m

-- | /O(1)/. The empty map. 
empty :: IntMapEq a
empty = IntMapEq IntMap.empty

-- |  /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. The injectivity invarant is /not/ enforced.
insert :: Int -> a -> IntMapEq a -> IntMapEq a
insert k a (IntMapEq m) = IntMapEq (IntMap.insert k a m)

-- |  /O(n * min(n,W))/. Insert a new key\/value pair in the map, if it is either
-- a new key, or agrees with the present value. If not, an error is signalled using
-- a 'Left' return value.
checkInsert :: Eq a => Int -> a -> IntMapEq a -> Either String (IntMapEq a)
checkInsert i b m = case lookup i m of
                      Nothing -> if memberR b m 
                                   then Left "Update violates unequality."
                                   else Right (insert i b m)
                      Just c  -> if b==c 
                                   then Right m 
                                   else Left "Update violates equality."

-- | /O(min(n,W))/. Is the key a member of the map?
member :: Int -> IntMapEq a -> Bool
member k (IntMapEq m) = IntMap.member k m

-- |  /O(n * min(n,W))/. Is the value a member of the map?
memberR :: Eq a => a -> IntMapEq a -> Bool
memberR a (IntMapEq m) = elem a (IntMap.elems m)

-- | /O(min(n,W))/. Lookup the value at a key in the map.
lookup :: Int -> IntMapEq a -> Maybe a
lookup k (IntMapEq m) = IntMap.lookup k m

-- | /O(n * min(n,W))/. Lookup the key at a value in the map.
lookupR :: Eq a => a -> IntMapEq a -> Maybe Int
lookupR a (IntMapEq m) = Prelude.lookup a (map (\(k,a) -> (a,k)) (IntMap.toList m))

-- | /O(m * n * min(n,W))/. The union of two maps. It prefers the first map
-- when duplicate keys are encountered. If the injectivity invarant is violated,
-- an error is signaled with a 'Left' return value.
union :: Eq a => IntMapEq a -> IntMapEq a -> Either String (IntMapEq a)
union h (IntMapEq m) = IntMap.foldWithKey f (Right h) m
  where f j a (Right h) = if member j h 
                            then Right h 
                            else if memberR a h
                                   then Left "Update violates unequality."
                                   else Right (insert j a h)
        f j a l         = l

-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: IntMapEq a -> [(Int,a)]
toList (IntMapEq m) = IntMap.toList m
