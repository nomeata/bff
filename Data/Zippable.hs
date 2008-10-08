{-# OPTIONS_GHC  -XTemplateHaskell -XFlexibleInstances -XFlexibleContexts -fallow-undecidable-instances  #-}

module Data.Zippable (
	module	Data.Zippable.Definition
) where


import Data.Zippable.Definition
import Data.DeriveTH
import Data.Derive.Zippable

$(derive makeZippable ''Maybe)
$(derive makeZippable ''[])

