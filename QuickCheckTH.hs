{-# LANGUAGE TemplateHaskell #-}

module QuickCheckTH
	( runTestGroup
	) where

import Language.Haskell.TH
import Test.QuickCheck

runTestGroup :: String -> [Name] -> ExpQ
runTestGroup group tests = doE $
	( noBindS $ appE (varE 'putStrLn) (litE (stringL ("Test Group \"" ++ group ++ "\"")))
	) : concatMap (\test ->
		[ noBindS $ appE (varE 'putStr) (litE (stringL (" * " ++ nameBase test ++ " ")))
		, noBindS $ appE (varE 'quickCheck) (varE test)
		]
	) tests
