{-# OPTIONS_GHC  -XTemplateHaskell  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SimpleTree
-- 
-- Maintainer  :  Janis Voigtlaender
-- Stability   :  experimental
--
-- A very simple binary Tree data structure.
-----------------------------------------------------------------------------
module SimpleTree where

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
