{-# LANGUAGE OverloadedStrings #-}
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

module Diagrams.HeatMap.Util where

import           Data.Maybe
import           Data.Monoid (mappend,mempty)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.RealFloat
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Diagrams.HeatMap.Impl
import           Diagrams.HeatMap.Type

makeDefualtPara :: ClustOpt -> Dataset -> Para
makeDefualtPara opt dataset = undefined

renderDataset :: Dataset -> TL.Text
renderDataset dataset = 
    toLazyText $ go (tVecToB (fromText sep) 0) 0
  where
    sep = "\t"
    (<>) = mappend
    i = nRow . datM $ dataset
    j = nCol . datM $ dataset
    cNVec = fromMaybe (V.generate j (T.pack . show)) $
            colNames dataset
    rNVec = fromMaybe (V.generate i (T.pack . show)) $
            rowNames dataset
    vec = case order $ datM dataset of
        RowMajor -> dat $ datM dataset
        ColumnMajor -> dat $ changeOrder $ datM dataset
    dVecToB b idx v | idx < j =
        let b' = b <> realFloat (v `atUV` idx) <>
                 fromText sep
            idx' = idx + 1
        in dVecToB b' idx' v
                    | otherwise = b <> fromText "\n"
    tVecToB b idx | idx < j =
        let b' = b <> fromText (cNVec `atV` idx) <>
                 fromText sep
            idx' = idx + 1
        in tVecToB b' idx'
                  | otherwise = b <> fromText "\n"
    go b idx | idx < i =
        let b' = fromText (rNVec `atV` idx) <>
                 fromText sep <>
                 dVecToB mempty 0 (UV.slice (idx*j) j vec)
            idx' = idx + 1
        in go b' idx'
             | otherwise = b

