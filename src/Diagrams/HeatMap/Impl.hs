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
module Diagrams.HeatMap.Impl where

import           Data.Colour
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Diagrams.HeatMap.Type

atV :: V.Vector a -> Int -> a
{-# INLINE atV #-}
atV = (V.!)

atUV :: UV.Unbox a => UV.Vector a -> Int -> a
{-# INLINE atUV #-}
atUV = (UV.!)

chooseColor :: Para -> Double -> Colour Double
{-# INLINE chooseColor #-}
chooseColor para v =
    let ColorVal vMin vMean vMax = colorVal para
    in case colorOpt . clustOpt $ para of
        Two lC hC -> blend ((v-vMin)/(vMax-vMin)) hC lC
        Three lC mC hC ->
            let vH = vMax - vMean
                vL = vMean - vMin
            in if v > vMean
               then blend ((v-vMean)/vH) hC mC
               else blend ((vMean-v)/vL) lC mC

changeOrder :: Matrix -> Matrix
{-# INLINE changeOrder #-}
changeOrder m =
    let i = nRow m
        j = nCol m
        v = dat m
    in case order m of
        RowMajor ->
            let v' = UV.generate (UV.length v)
                     (\idx ->
                       let (r,c) = idx `divMod` i
                           idx' = c*j+r
                       in v `atUV` idx'
                     )
            in m { order = ColumnMajor, dat = v'}
        ColumnMajor ->
            let v' = UV.generate (UV.length v)
                     (\idx ->
                       let (r,c) = idx `divMod` j
                           idx' = c*i+r
                       in v `atUV` idx'
                     )
            in m { order = RowMajor, dat = v'}
