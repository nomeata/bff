{-# LANGUAGE PatternGuards, TemplateHaskell #-}

module Data.Derive.Zippable (makeZippable) where

import Language.Haskell.TH.All
import Control.Monad
import Control.Monad.Either
import NormalizeData
import Data.Zippable

makeZippable :: Derivation
makeZippable = derivation zip' "Zippable"

zip' dat | (dataArity dat) /= 1 = error "Can not handle types with artity not zero."
         | otherwise            =
	   let typeName | dataName dat == "[]" = ConT ''[] -- doesn’t work!
                        | otherwise            = lK (dataName dat) []
 	       head = InstanceD [] (AppT (ConT (mkName "Zippable")) typeName)
               func = funN "tryZipWith'" (
			map mkClause (dataCtors dat) ++
			whenP (length (dataCtors dat) > 1)
                              [Clause [WildP, WildP, WildP]
                                  (NormalB (app (VarE 'throwCError) [LitE (StringL
						"Structure mismatch in tryZip"
					)]))
			          []
                              ]
			)
	   in [ head [ func ] ]


mkClause :: CtorDef -> Clause
mkClause con = sclause [vr "func", lK (ctorName con) pat1names, lK (ctorName con) pat2names]
		       (collectZips (map zipVar [0..ctorArity con-1]) (lK (ctorName con)))

  where varnames number prefix = map (vr . (prefix++) . show) [1..number]
	pat1names, pat2names :: Valcon a => [a]
        pat1names = varnames (ctorArity con) "x"
        pat2names = varnames (ctorArity con) "y"

	collectZips actions join =
		 DoE $ zipWith BindS zipnames actions ++
  	               [ NoBindS $ lK "return" [join zipnames]]
	  where zipnames :: Valcon a => [a]
	        zipnames  = varnames (length actions) "z"
       
	zipVar n  = app (zip (ctorTypes con !! n)) [pat1names !! n, pat2names !! n]

        tupMerge ts =   let pat1names, pat2names, zipnames :: Valcon a => [a]
			    n = length ts
			    pat1names = varnames n "x"
			    pat2names = varnames n "y"
			    zipnames  = varnames n "z"
                        in  LamE [x,y] $ CaseE (TupE [x,y]) [ Match
				(TupP [TupP pat1names, TupP pat2names])
                                (NormalB
                                    (collectZips
				        (map (\i -> app (zip (ts !! i)) [pat1names !! i, pat2names !! i]) [0..n-1])
					TupE 
                                    )
				)
				[]
			     ]

        zip ctype = case ctype of 
		 VarT _ ->
			lK "func" []
		 ConT _ ->
			(VarE 'checkEquality)
		 -- If we have tuples, we basically have to repeat the currenct procedure
		 -- Using a case expressen, we can safly re-use variables names, even
		 -- with nested tuples.
		 t@(AppT _ _) | (ht, ts) <- typeApp t, isTupleT ht ->
			tupMerge ts 
		 AppT t (VarT _) | not (tyHasVar t) -> 
			lK "tryZipWith'" [vr "func"]
		 t@(AppT _ _) | not (tyHasVar t) -> 
			(VarE 'checkEquality)
		 t@(AppT _ ct) ->
			lK "tryZipWith'" [zip ct]
		 ForallT _ _ _ ->
			error "Types with forall not supported by Zippable deriver."
		 TupleT _ ->
			error "Types with tuples not expected here."
		 ArrowT ->
			error "Arrow types not supported by Zippable deriver."
		 ListT  ->
			error "List types not supported by Zippable deriver."

	x,y :: Valcon a => a
	x = vr "x"
	y = vr "y"

tyHasVar t = case t of
		 VarT _ -> True
		 ConT _ -> False
		 AppT t1 t2 -> tyHasVar t1 || tyHasVar t2
		 ForallT _ _ _ -> error "Types with forall not supported by Zippable deriver."
		 TupleT _ -> False
		 ArrowT -> False
		 ListT  -> False

whenP :: MonadPlus m => Bool -> m a -> m a
whenP True  x = x
whenP False _ = mzero

-- | Functions used in the derived code
checkEquality x y = if (x == y) then return x
				else throwCError "Non-Data value mismatch in tryZip"


-- | Extract a 'DataDef' value from a type using the TH 'reify'
-- framework.
deriveOne :: Name -> Q DataDef
deriveOne x = liftM extract (reify x)

extract (TyConI decl) = normData decl
extract _ = error $ "Data.Derive.TH.deriveInternal: not a type!"
