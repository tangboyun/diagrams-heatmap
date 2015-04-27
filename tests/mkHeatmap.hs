{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Main where

import           Control.Arrow
import           Data.Colour.Names
import           Data.Colour.Palette.BrewerSet
import qualified Data.HashMap.Strict           as H
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Read                as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
import           Diagrams.Backend.Cairo
import           Diagrams.HeatMap
import           Diagrams.HeatMap.Type
import           Diagrams.Prelude
import           Statistics.Quantile
import           Statistics.Sample
import           System.Environment
import           System.FilePath


cluDiffOpt :: ClustOpt
cluDiffOpt = ClustOpt
  { colorOpt = color2
  , rowCluster = Nothing -- Just (eucDis,UPGMA,LeftTree)
  , colCluster = Just (eucDis,UPGMA,BottomTree)}

color1 = let cSet = brewerSet RdBu 11
         in Three (cSet !! 9) white (cSet !! 1)

color2 = Three green black red

mkPara w h i j (a,b,c) = Para
  { clustOpt = cluDiffOpt
  , colorVal = ColorVal a b c
  , matrixHeight = h
  , matrixWidth = w
  , rowTreeHeight = 0.2 * min w h
  , colTreeHeight = 0.2 * min w h
  , rowFontSize = 0.8 * h / i
  , colFontSize = 0.6 * w / j
  , legendFontSize = 0.5 * w * 0.1
  , fontName = "Arial"
  , colorBarPos = Horizontal
  , tradeOff = Quality
  , colTreeLineWidth = 0.5
  , rowTreeLineWidth = 0.5
  }

eucDis :: (UV.Unbox a, Num a) => UV.Vector a -> UV.Vector a -> a
eucDis vec1 vec2 = UV.sum $ UV.zipWith (\v1 v2 -> (v1-v2)*(v1-v2)) vec1 vec2

toZscore :: UV.Vector Double -> UV.Vector Double
toZscore vec =
    let (m,v) = meanVarianceUnb vec
    in UV.map (\e -> (e - m) / sqrt v ) vec

parseTSV :: Bool -> FilePath -> FilePath -> IO Dataset
parseTSV doNormalization datFile labelFile = do
    sampleHash <- T.readFile labelFile >>=
                  return . H.fromList .
                  map (((!! 0) &&& (!! 1)) . T.split (== '\t')) .
                  T.lines .
                  T.filter (/= '\r')
    T.readFile datFile >>=
        return .
        (\(h:ts) ->
          let sIDs = V.fromList $ tail h
              grIDs = V.map (sampleHash `myIdx`) sIDs
              gIDs = V.fromList $ map head ts
              j = V.length sIDs
              i = V.length gIDs
              func = if doNormalization
                     then toZscore
                     else id
              datum = UV.concat $ map (func . UV.fromList . map (fst . fromRight . T.double) . tail) ts
              m = Matrix i j RowMajor datum
          in Dataset (Just gIDs) (Just sIDs) (Just grIDs) m
        ) . map (T.split (== '\t')) . T.lines . T.filter (/= '\r')
  where
    myIdx h k = if k `H.member` h
                then h H.! k
                else error $ show k
    fromRight :: Show a => Either a b -> b
    fromRight (Right b) = b
    fromRight (Left a) = error $ show a

main :: IO ()
main = do
   doNormal:w:h:datFile:labelFile:_ <- getArgs
   dataset <- parseTSV (read doNormal) datFile labelFile
   let vMin = continuousBy medianUnbiased 1 100 $ dat $ datM $ dataset
       vMean = if (read doNormal)
               then 0
               else continuousBy medianUnbiased 50 100 $ dat $ datM $ dataset
       vMax = continuousBy medianUnbiased 99 100 $ dat $ datM $ dataset
       j = fromIntegral $ nCol $ datM dataset
       i = fromIntegral $ nRow $ datM dataset
       para = mkPara (read w) (read h) i j (vMin,vMean,vMax)
   renderCairo (replaceExtension datFile "pdf") (mkWidth 600) $ pad 1.03 $ fst $ plotHeatMap para dataset

