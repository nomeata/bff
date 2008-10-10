{-# OPTIONS_GHC  -XTemplateHaskell -XFlexibleInstances -XFlexibleContexts -fallow-undecidable-instances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Zippable
-- Copyright   :  (c) 2008 Joachim Breitner
-- 
-- Maintainer  :  Joachim Breitner
-- Stability   :  experimental
--
-- Class of data structures that can match equal shapes and combine the values.
-----------------------------------------------------------------------------

module Data.Zippable (
	module	Data.Zippable.Definition,
	makeZippable
) where


import Data.Zippable.Definition
import Data.DeriveTH
import Data.Derive.Zippable

$(derive makeZippable ''Maybe)
$(derive makeZippable ''[])

