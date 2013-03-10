{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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

import Diagrams.HeatMap
import Diagrams.HeatMap.Type
import Diagrams.Backend.Cairo.CmdLine
--import Diagrams.Backend.SVG.CmdLine
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import Data.Colour.Names
import Data.Clustering.Hierarchical
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.List
import Diagrams.Prelude
import Diagrams.HeatMap.Unsafe
import Diagrams.HeatMap.Impl
import Data.Function

inputFile = "/tmp/testData.txt"

cluDiffOpt = ClustOpt
  { colorOpt = Three blue yellow red
  , rowCluster = Nothing -- Just (eucDis,UPGMA,LeftTree)
  , colCluster = Just (eucDis,UPGMA,TopTree)}
  
para' = Para
  { clustOpt = cluDiffOpt
  , colorVal = ColorVal 5 10 15
  , matrixHeight = 800
  , matrixWidth = 1000
  , rowTreeHeight = 200
  , colTreeHeight = 200
  , rowFontSize = 1000 / 120
  , colFontSize = 1000 / 90
  , fontName = "Times New Roman"
  , colorBarPos = Vertical
  , tradeOff = Performance
  }

minMax :: UV.Vector Double -> (Double,Double)
minMax v =
    UV.foldl
    (\t@(vMin,vMax) v ->
      if v < vMin
      then (v,vMax)
      else if v > vMax
           then (vMin,v)
           else t) (1/0,-1/0) v

parseTSV :: FilePath -> IO Dataset
parseTSV fp =
    T.readFile fp >>=
    return .
    (\(h:ts) ->
      let tts = map extractText $ tail h
          sIDs = V.fromList $ map (T.toStrict . fst) tts
          grIDs = V.fromList $ map (T.toStrict . snd) tts
          gIDs = V.fromList $ map (T.toStrict . head) ts
          grIDV = V.fromList $ nub $ sort $ map snd tts
          j = V.length sIDs
          i = V.length gIDs
          dat = UV.concat $ map (UV.fromList . map (fst . fromRight . T.double) . tail) ts
          m = Matrix i j RowMajor dat
      in if V.length grIDV > 1
         then Dataset (Nothing) (Just sIDs) (Just grIDs) m
         else Dataset (Nothing) (Just sIDs) Nothing m
      ) . map (T.split (== '\t')) . T.lines . T.filter (/= '\r')
  where
    fromRight :: Either a b -> b
    fromRight (Right b) = b
    fromRight _ = error "fromRight"
    extractText t =
        (\ts -> (head ts,T.intercalate " " $ tail ts)) $
        T.splitOn ", " $
        T.tail $ T.dropWhile (/= '[') $ T.takeWhile (/= ']') t

main = do
   dataset <- parseTSV inputFile
   -- print $ rowNames dataset
   let (_d :: Diagram Cairo R2,dset) = toHeatMap para' dataset
       i = nRow $ datM dset
       j = nCol $ datM dset
       m = changeOrder $ datM dataset
       v = dat m 
       vec = dat $ changeOrder $ datM dset
       vvec = V.fromList $ map (\idx -> UV.slice (idx*i) i v)  [0..j-1]
       v' = UV.concat $ V.toList $ V.map (flip UV.unsafeBackpermute idxV) vvec 
       m' = m {dat=v',order= ColumnMajor}
       dataset' = dataset {datM = m'}
       idxV = UV.fromList $ map fst $ sortBy (flip compare `on` snd) $
              UV.toList $ UV.indexed $ UV.slice 0 i vec
       (dia :: Diagram Cairo R2,_) = toHeatMap para' dataset'
--   print $ UV.length $ dat $ datM d'
--   print $ rowNames d'
--   print $ colNames d'
--   print $ colLabels d'
  --   T.writeFile "/tmp/test.txt" $ renderDataset "\t" d'
--   defaultMain $ pad 1.03 $ dia
   let p = plotMatrixCairo para' $! datM dataset
   print p
   --defaultMain $ pad 1.03 $ image (plotMatrixCairo para' $! datM dataset) (matrixWidth para') (matrixHeight para')
