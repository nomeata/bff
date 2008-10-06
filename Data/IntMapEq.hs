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

empty :: IntMapEq a
empty = IntMapEq IntMap.empty

insert :: Int -> a -> IntMapEq a -> IntMapEq a
insert k a (IntMapEq m) = IntMapEq (IntMap.insert k a m)

checkInsert :: Eq a => Int -> a -> IntMapEq a -> Either String (IntMapEq a)
checkInsert i b m = case lookup i m of
                      Nothing -> if memberR b m 
                                   then Left "Update violates unequality."
                                   else Right (insert i b m)
                      Just c  -> if b==c 
                                   then Right m 
                                   else Left "Update violates equality."

member :: Int -> IntMapEq a -> Bool
member k (IntMapEq m) = IntMap.member k m

memberR :: Eq a => a -> IntMapEq a -> Bool
memberR a (IntMapEq m) = elem a (IntMap.elems m)

lookup :: Int -> IntMapEq a -> Maybe a
lookup k (IntMapEq m) = IntMap.lookup k m

lookupR :: Eq a => a -> IntMapEq a -> Maybe Int
lookupR a (IntMapEq m) = Prelude.lookup a (map (\(k,a) -> (a,k)) (IntMap.toList m))

union :: Eq a => IntMapEq a -> IntMapEq a -> Either String (IntMapEq a)
union h (IntMapEq m) = IntMap.foldWithKey f (Right h) m
  where f j a (Right h) = if member j h 
                            then Right h 
                            else if memberR a h
                                   then Left "Update violates unequality."
                                   else Right (insert j a h)
        f j a l         = l

toList :: IntMapEq a -> [(Int,a)]
toList (IntMapEq m) = IntMap.toList m
