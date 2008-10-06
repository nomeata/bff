{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  QuickCheck
-- Copyright   :  (c) 2008 Joachim Breitner
-- 
-- Maintainer  :  Joachim Breitner <mail@joachim-brietner.de>
-- Stability   :  experimental
--
-- This modules describes the properties of the various IntMaps used
-- by Janis Voigtlaender for his "Bidirectionalization For Free" work.
--
-- It also has a main function and can be run to verify these properties.
--
-----------------------------------------------------------------------------

module QuickCheck (
	-- * IntMap tests
	  prop_Empty
	, prop_Insert
	, prop_Member
	, prop_Union
	, prop_FromAscList
	
	-- * IntMapEq tests
	, prop_Injectivity_Eq
	, prop_Empty_Eq
	, prop_Insert_Eq
	, prop_Member_Eq
	, prop_Union_Good_Eq
	, prop_Union_Bad_Eq
	, prop_LookupR_Eq
	, prop_MemberR_Eq
	, prop_CheckInsert_Good_Eq
	, prop_CheckInsert_Bad1_Eq
	, prop_CheckInsert_Bad2_Eq

	-- * IntMapOrd tests
	, prop_Orderdness_Ord
	, prop_CheckInsert_Good_Ord
	, prop_CheckInsert_Bad1_Ord
	, prop_CheckInsert_Bad2_Ord
	, prop_CheckInsert_Bad3_Ord
	, prop_Union_Good_Ord 
	, prop_Union_Bad_Ord
	) where 

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntMapEq (IntMapEq(..))
import qualified Data.IntMapEq as IntMapEq
import Data.IntMapOrd (IntMapOrd(..))
import qualified Data.IntMapOrd as IntMapOrd
import Test.QuickCheck
import Test.QuickCheck.Batch
import Test.QuickCheck.Poly
import Data.List
import Control.Monad.Fix
import QuickCheckTH

-- Tests for IntMap

instance Arbitrary a => Arbitrary (IntMap a) where
	arbitrary = IntMap.fromList `fmap` arbitrary
	coarbitrary = error "coabitrary not defined for IntMap a"

-- | Looking up on an empty IntMap returns 'Nothing'
prop_Empty :: Int -> Bool
prop_Empty i =
	IntMap.lookup i IntMap.empty == (Nothing :: Maybe ALPHA)

-- | Lookup up a just inserted key returns it's new value,
--   while lookup up after inserting with a different key does
--   not change the lookup
prop_Insert :: IntMap ALPHA -> Int -> ALPHA -> Int -> Bool
prop_Insert m i a j =
	IntMap.lookup j (IntMap.insert i a m) ==
		if i == j then Just a
                          else IntMap.lookup j m

-- | A key is a member exactly when 'IntMap.lookup' returns not 
--   'Nothing'
prop_Member :: IntMap ALPHA -> Int -> Bool
prop_Member m i =
	IntMap.member i m ==
		case IntMap.lookup i m of
			Just _  -> True
			Nothing -> False

-- | When lookup up a key from a union of two IntMaps, the returned value
--   comes from the first IntMap, if it exists there, otherwise from the other 
--   one, if it exists there.
prop_Union :: IntMap ALPHA -> IntMap ALPHA -> Int -> Bool
prop_Union m m' i =
	asMaybe (IntMap.lookup i (IntMap.union m m')) ==
        	if IntMap.member i m then IntMap.lookup i m
                                     else IntMap.lookup i m'

-- | Lookup up a key from a IntMap built by fromAscList is equivalent
--   to using 'Data.List.lookup' on the original list.
prop_FromAscList :: AscList ALPHA -> Int -> Bool
prop_FromAscList (AscList asc) i =
	lookup i asc == IntMap.lookup i (IntMap.fromAscList asc)

-- Tests for IntMapEq

instance (Arbitrary a, Eq a) => Arbitrary (IntMapEq a) where
	-- Here we spread out the values of the Map compared to the size,
	-- so we get some conflicts, but not too much.
	arbitrary = sized $ \n -> resize (4*n) $ do
			ints <- uniqueVector n
			as   <- uniqueVector n
			-- no fromList exposed in IntMapEq
			return $ foldr (uncurry IntMapEq.insert) IntMapEq.empty (zip ints as)
	coarbitrary = error "coabitrary not defined for IntMapEq a"

-- | This is more a test of the Arbitrary instance, than of the implementation
--   itself, but also exmplains the additional invariant in IntMapEq
prop_Injectivity_Eq :: IntMapEq ALPHA -> Int -> Int -> Property
prop_Injectivity_Eq m i j = 
	i /= j ==>
		IntMapEq.lookup i m == Nothing ||
	        IntMapEq.lookup j m == Nothing ||
		IntMapEq.lookup i m /= IntMapEq.lookup j m 

-- | Looking up on an empty IntMapEq returns 'Nothing'
prop_Empty_Eq :: Int -> Bool
prop_Empty_Eq i =
	IntMapEq.lookup i IntMapEq.empty == (Nothing :: Maybe ALPHA)

-- | Lookup up a just inserted key returns it's new value,
--   while lookup up after inserting with a different key does
--   not change the lookup.
prop_Insert_Eq :: IntMapEq ALPHA -> Int -> ALPHA -> Int -> Property
prop_Insert_Eq m i a j =
	let l = IntMapEq.toList m
	in (i,a) `elem` l || all (\(j,a') -> j /= i && a /= a') l  ==>
		IntMapEq.lookup j (IntMapEq.insert i a m) ==
			if i == j then Just a
                          else IntMapEq.lookup j m

-- | A key is a member exactly when 'IntMap.lookup' returns not 
--   'Nothing'
prop_Member_Eq :: IntMapEq ALPHA -> Int -> Bool
prop_Member_Eq m i =
	IntMapEq.member i m ==
		case IntMapEq.lookup i m of
			Just _  -> True
			Nothing -> False


-- It’s actually hard to get enough samples of good unions

-- | A unions of two IntMapEq, where the Union is actually defined, behaves 
--   as described in 'prop_Union'
prop_Union_Good_Eq :: IntMapEq ALPHA -> IntMapEq ALPHA -> Int -> Property
prop_Union_Good_Eq m m' i =
	not (conflicting_Eq m m') ==>
		case IntMapEq.union m m' of
			Right u -> IntMapEq.lookup i u ==
					if IntMapEq.member i m then IntMapEq.lookup i m
							       else IntMapEq.lookup i m'
			Left _  -> False

-- | A unions of two IntMapEq, where the Union is not defined, becaues there are
--   conflicting values, returns an 'Left' value.
prop_Union_Bad_Eq :: IntMapEq ALPHA -> IntMapEq ALPHA -> Property
prop_Union_Bad_Eq m m' =
	conflicting_Eq m m' ==>
		case IntMapEq.union m m' of
			Left "Update violates unequality." -> True
			Right _                            -> False

-- | Tests whether two IntMapsEq would conflict upon an merge, that is, whether a
--   key-value pair exists, which is not overritten by a key in the first map, but
--   whose value appears in the firt map already.
conflicting_Eq :: (Eq a) => IntMapEq a -> IntMapEq a -> Bool
conflicting_Eq m m' =
	   let l  = IntMapEq.toList m
               l' = IntMapEq.toList m'
           in any (\(j,a') -> any (\(_,a) -> a == a') l && all (\(i,_) -> i/=j) l) l'

-- | Lookup up a just inserted value returns it's new key,
--   while lookup up after inserting with a different value does
--   not change the lookup.
prop_LookupR_Eq :: IntMapEq ALPHA -> ALPHA -> Int -> ALPHA -> Property
prop_LookupR_Eq m a i a' =
	-- otherwise insert is not defined
	let l = IntMapEq.toList m
	in (i,a') `elem` l || all (\(j,a'') -> j /= i && a' /= a'') l  ==>
		case IntMapEq.checkInsert i a' m of
			Right m' -> IntMapEq.lookupR a m' == 
				if a == a' then Just i
					   else IntMapEq.lookupR a m
                        Left _   -> False

-- | A value is a member exactly when 'IntMapEq.lookupR' returns not 'Nothing'
prop_MemberR_Eq :: IntMapEq ALPHA -> ALPHA -> Bool
prop_MemberR_Eq m a =
	IntMapEq.memberR a m ==
		case IntMapEq.lookupR a m of
			Just _  -> True
			Nothing -> False

-- | For a value where 'IntMapEq.checkInsert' is defined, this behavas 
--   as in 'prop_Insert'.
prop_CheckInsert_Good_Eq :: IntMapEq ALPHA -> Int -> ALPHA -> Property
prop_CheckInsert_Good_Eq m i a =
	let l = IntMapEq.toList m
	in (i,a) `elem` l || all (\(j,a') -> j /= i && a /= a') l  ==>
		case IntMapEq.checkInsert i a m of
			Right m' -> IntMapEq.lookup i m' == Just a
                        Left _   -> False
		
-- | For values that violate the equality condition of updates, 'IntMapEq.checkInsert'
--   returns an error message.
prop_CheckInsert_Bad1_Eq :: IntMapEq ALPHA -> Int -> ALPHA -> Property
prop_CheckInsert_Bad1_Eq m i a =
	let l = IntMapEq.toList m
	in any (\(j,a') -> i == j && a /= a') l ==>
		case IntMapEq.checkInsert i a m of
			Right _                            -> False
                        Left "Update violates equality."   -> True

-- | For values that violate the injectivity condition of the IntMapEq,
--   'IntMapE.checkInsert' returns an error message.
prop_CheckInsert_Bad2_Eq :: IntMapEq ALPHA -> Int -> ALPHA -> Property
prop_CheckInsert_Bad2_Eq m i a =
	let l = IntMapEq.toList m
	in all (\(j,_) -> i /= j) l && any (\(_,a') -> a == a') l  ==>
		case IntMapEq.checkInsert i a m of
			Right _                            -> False
                        Left "Update violates unequality." -> True

-- Tests for IntMapOrd

instance (Arbitrary a, Ord a) => Arbitrary (IntMapOrd a) where
	-- Here we spread out the values of the Map compared to the size,
	-- so we get some conflicts, but not too much.
	arbitrary = sized $ \n -> resize (2*n) $ do
			ints <- ascendingVector n
			as   <- ascendingVector n
			-- no fromList exposed in IntMapOrd
			return $ IntMapOrd.fromAscPairList (zip ints as)
	coarbitrary = error "coabitrary not defined for IntMapOrd a"

-- | This is more a test of the Arbitrary instance, than of the implementation
--   itself, but also exmplains the additional invariant in IntMapOrd
prop_Orderdness_Ord :: IntMapOrd OrdALPHA -> Int -> Int -> Property
prop_Orderdness_Ord m i j = 
	i < j ==>
		IntMapOrd.lookup i m == Nothing ||
	        IntMapOrd.lookup j m == Nothing ||
		IntMapOrd.lookup i m < IntMapOrd.lookup j m 

-- | For a value where 'IntMapOrd.checkInsert' is defined, this behavas 
--   as in 'prop_Insert'.
prop_CheckInsert_Good_Ord :: IntMapOrd OrdALPHA -> Int -> OrdALPHA -> Property
prop_CheckInsert_Good_Ord m i a =
	let l = IntMapOrd.toList m
            (is, as) = unzip l
	in (i,a) `elem` l || all (\(j,a') -> j /= i && a /= a') l 
	   && length ((filter (<i)) is) == length ((filter (<a)) as) ==>
		case IntMapOrd.checkInsert i a m of
			Right m' -> IntMapOrd.lookup i m' == Just a
                        Left _   -> False

-- | For values that violate the equality condition of updates, 'IntMapOrd.checkInsert'
--   returns an error message.
prop_CheckInsert_Bad1_Ord :: IntMapOrd OrdALPHA -> Int -> OrdALPHA -> Property
prop_CheckInsert_Bad1_Ord m i a =
	let l = IntMapOrd.toList m
	in any (\(j,a') -> i == j && a /= a') l ==>
		case IntMapOrd.checkInsert i a m of
			Right _                            -> False
                        Left "Update violates equality."   -> True

-- | For values that violate the injectivity condition of the IntMapOrd,
--   'IntMapE.checkInsert' returns an error message.
prop_CheckInsert_Bad2_Ord :: IntMapOrd OrdALPHA -> Int -> OrdALPHA -> Property
prop_CheckInsert_Bad2_Ord m i a =
	let l = IntMapOrd.toList m
	in all (\(j,_) -> i /= j) l && any (\(_,a') -> a == a') l  ==>
		case IntMapOrd.checkInsert i a m of
			Right _                            -> False
                        Left "Update violates unequality." -> True

-- | For values that violate the monotony condition of the IntMapOrd,
--   'IntMapE.checkInsert' returns an error message.
prop_CheckInsert_Bad3_Ord :: IntMapOrd OrdALPHA -> Int -> OrdALPHA -> Property
prop_CheckInsert_Bad3_Ord m i a =
	let l = IntMapOrd.toList m
            (is, as) = unzip l
	in all (\(j,a') -> j /= i && a /= a') l 
	   && length ((filter (<i)) is) /= length ((filter (<a)) as) ==>
		case IntMapOrd.checkInsert i a m of
			Right _                                -> False
                        Left "Update violates relative order." -> True

-- It’s actually hard to get enough samples of good unions


-- | A unions of two IntMapOrd, where the Union is actually defined, behaves 
--   as described in 'prop_Union'
prop_Union_Good_Ord :: IntMapOrd OrdALPHA -> IntMapOrd OrdALPHA -> Int -> Property
prop_Union_Good_Ord m m' i =
	not (conflicting_Ord m m') ==>
		case IntMapOrd.union m m' of
			Right u -> IntMapOrd.lookup i u ==
					if IntMapOrd.member i m then IntMapOrd.lookup i m
							        else IntMapOrd.lookup i m'
			Left _  -> False

-- | A unions of two IntMapOrd, where the Union is not defined, becaues there are
--   conflicting values, returns an 'Left' value.
prop_Union_Bad_Ord :: IntMapOrd OrdALPHA -> IntMapOrd OrdALPHA -> Property
prop_Union_Bad_Ord m m' =
	conflicting_Ord m m' ==>
		case IntMapOrd.union m m' of
			Left  _ -> True
			Right _ -> False

-- | Tests whether two IntMapsOrd would conflict upon an merge, that is, whether a
--   key-value pair exists, which is not overritten by a key in the first map, but
--   whose value appears in the firt map already, or when their keys and values do
--   not merge pairwise.
conflicting_Ord :: (Ord a) => IntMapOrd a -> IntMapOrd a -> Bool
conflicting_Ord m m' =
	   let l  = IntMapOrd.toList m
               l' = IntMapOrd.toList m'
               (is,as)   = unzip l
               (is',as') = unzip $ filter (\(j,_) -> all (\(i,_) -> i /= j) l) l'
           in any (\(j,a') -> any (\(_,a) -> a == a') l && all (\(i,_) -> i/=j) l) l' ||
              badMerge (sort is) (sort as) (sort is') (sort as')
  where badMerge [] [] _  _  = False
        badMerge _  _  [] [] = False
        badMerge (a:as) (b:bs) (c:cs) (d:ds) | a == c = badMerge (a:as) (b:bs) cs ds
				             | a < c = b > d || badMerge as bs (c:cs) (d:ds)
                                             | c < a = d > b || badMerge (a:as) (b:bs) cs ds

-- Helpers

-- | some type signatures are genaral in the return monad. To easily fix then to 
--   maybe, this helper is used.
asMaybe :: Maybe a -> Maybe a
asMaybe = id

-- | similar to Test.Quickcheck.vector, but with unique elements
uniqueVector :: (Arbitrary a, Eq a) => Int -> Gen [a]
uniqueVector 0 = return []
uniqueVector n = do tail <- uniqueVector (n-1)
		    head <- searchFor (`notElem` tail) arbitrary
		    return (head:tail)

-- | similar to Test.Quickcheck.vector, but with strictly ascending elements
ascendingVector :: (Arbitrary a, Ord a) => Int -> Gen [a]
ascendingVector 0 = return []
ascendingVector n = do head <- resize 0 arbitrary
		       tail <- go head (n-1)
		       go head n
  where go _ 0 = return []
        go l m = do head <- searchFor (> l) (resize (3*(n-m+1)) arbitrary)
                    tail <- go head (m-1)
		    return (head:tail)

-- | finds a value satisfiying the predicate. WARNING: May not terminate if there is no
--   such value, or the value is very unlikely to find.
searchFor :: (Monad m) => (a -> Bool) -> m a -> m a
searchFor pred gen = do x <- gen
                        if pred x then return x
                                  else searchFor pred gen

newtype AscList a = AscList [(Int, a)] deriving (Show)

instance Arbitrary a => Arbitrary (AscList a) where
	arbitrary = do ascList <- sized $ \n -> resize (2*n) $ do 
		           ints <- ascendingVector n
		           as   <- vector n
		           return (zip ints as)
		       return (AscList ascList)
	coarbitrary = error "coabitrary not defined for AscList a"


main :: IO ()
main = do $( runTestGroup "IntMap"
		[ 'prop_Empty
		, 'prop_Insert
		, 'prop_Member
		, 'prop_Union
		, 'prop_FromAscList
		] )
          $( runTestGroup "IntMapEq"
		[ 'prop_Injectivity_Eq
		, 'prop_Empty_Eq
		, 'prop_Insert_Eq
		, 'prop_Member_Eq
		, 'prop_Union_Good_Eq
		, 'prop_Union_Bad_Eq
		, 'prop_LookupR_Eq
		, 'prop_MemberR_Eq
		, 'prop_CheckInsert_Good_Eq
		, 'prop_CheckInsert_Bad1_Eq
		, 'prop_CheckInsert_Bad2_Eq
		] )
          $( runTestGroup "IntMapOrd"
		[ 'prop_Orderdness_Ord
		, 'prop_CheckInsert_Good_Ord
		, 'prop_CheckInsert_Bad1_Ord
		, 'prop_CheckInsert_Bad2_Ord
		, 'prop_CheckInsert_Bad3_Ord
		, 'prop_Union_Good_Ord 
		, 'prop_Union_Bad_Ord
		] )
