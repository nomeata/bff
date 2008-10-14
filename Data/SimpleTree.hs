{-# OPTIONS_GHC  -XTemplateHaskell -XFlexibleInstances -XFlexibleContexts -fallow-undecidable-instances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SimpleTree
-- Copyright   :  (c) 2008 Janis Voigtländer
-- 
-- Maintainer  :  Janis Voigtländer
-- Stability   :  experimental
--
-- A very simple binary Tree data structure.
-----------------------------------------------------------------------------
module Data.SimpleTree where

import Data.Traversable
import Data.Foldable
import Data.Zippable
import Data.DeriveTH
import Data.Derive.Traversable
import Data.Derive.Zippable
import Control.Applicative

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq, Ord, Read)

$( derive makeTraversable ''Tree )

instance Foldable Tree where
  foldMap = foldMapDefault

instance Functor Tree where
  fmap = fmapDefault

$( derive makeZippable ''Tree )
