-----------------------------------------------------------------------------
-- |
-- Module :
-- Copyright : (c) 2013 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
--
--
-----------------------------------------------------------------------------

module Diagrams.HeatMap.Module
       ( module Data.Clustering.Hierarchical
       , module Data.Colour.Names
       , module Data.Colour.RGBSpace
       , module Data.Colour.RGBSpace.HSL
       , module Data.Colour.SRGB
       , module Data.Function
       , module Data.List
       , module Data.List.Split
       , module Data.Maybe
       , module Text.Printf
       , module Diagrams.TwoD.Image
       , module Diagrams.TwoD.Text
       , module Diagrams.Prelude
       , module Data.Foldable
       , module Control.Arrow
       ) where

import           Control.Arrow                hiding ((|||))
import           Data.Clustering.Hierarchical
import           Data.Colour.Names
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Colour.SRGB
import           Data.Foldable                (toList)
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Diagrams.Prelude             hiding (trace)
import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Text
import           Text.Printf
