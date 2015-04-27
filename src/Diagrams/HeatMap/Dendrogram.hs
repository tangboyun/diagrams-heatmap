{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module :
-- Copyright : (c) 2014 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
--
--
-----------------------------------------------------------------------------
module Diagrams.HeatMap.Dendrogram where

import           Control.Arrow                (second)
import           Data.Clustering.Hierarchical (Dendrogram (..))
import           Diagrams.Prelude

fixedWidth :: Double -> Dendrogram a -> (Dendrogram (a, Double), Double)
fixedWidth w = second (subtract half_w) . go half_w
    where
      half_w = w/2
      go !y (Leaf datum)   = (Leaf (datum, y), y + w)
      go !y (Branch d l r) = (Branch d l' r', y'')
          where
            (l', !y')  = go y  l
            (r', !y'') = go y' r

dendrogramPath :: Dendrogram Double -> Path V2 Double
dendrogramPath = mconcat . fst . go []
    where
      go acc (Leaf x)       = (acc, (x, 0))
      go acc (Branch d l r) = (path : acc'', pos)
        where
          (acc',  (!xL, !yL)) = go acc  l
          (acc'', (!xR, !yR)) = go acc' r

          path = fromVertices [ p2 (xL, yL)
                              , p2 (xL, d)
                              , p2 (xR, d)
                              , p2 (xR, yR)]
          pos  = (xL + (xR - xL) / 2, d)
