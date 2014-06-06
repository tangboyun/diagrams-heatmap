{-# LANGUAGE FlexibleContexts #-}
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

module Diagrams.HeatMap.Internal where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Diagrams.Dendrogram as D
import           Diagrams.HeatMap.Impl
import           Diagrams.HeatMap.Module
import           Diagrams.HeatMap.Type
import           Diagrams.HeatMap.Unsafe


toTree :: (Renderable (Path R2) b,Backend b R2)
       => Double
       -> Double
       -> Dendrogram a
       -> (Diagram b R2,Double)
toTree lineW elemWidth dendro =
    let (path,wid) = first (D.dendrogramPath . fmap snd) $
                     D.fixedWidth elemWidth dendro
        dia = stroke path # lw lineW # lc black # centerXY ===
              strutX wid ===
              strutY (1e-2 * h)
        h = case dendro of
            Leaf _ -> 0
            Branch d _ _ -> d
    in (dia,h)

            
clustering :: ClustOpt -> Matrix
           -> (Maybe (Dendrogram Int)  -- row dendro
             ,Maybe (Dendrogram Int)) -- col dendro
clustering cluOpt m =
    let vecOrd = order m
        i = nRow m
        j = nCol m
        colFunc (disFunc,linkage,_) =
            let v = case vecOrd of
                    ColumnMajor -> dat m
                    RowMajor -> dat $ changeOrder m
                vvec = V.fromList $
                        map (\idx -> UV.slice (idx*i) i v) [0..j-1]
            in dendrogram linkage [0..j-1]
               (disFunc `on` (vvec `atV`))
        rowFunc (disFunc,linkage,_) =
            let v = case vecOrd of
                    ColumnMajor -> dat $ changeOrder m
                    RowMajor -> dat m
                vvec = V.fromList $
                       map (\idx -> UV.slice (idx*j) j v) [0..i-1]
            in dendrogram linkage [0..i-1]
               (disFunc `on` (vvec `atV`))
    in (fmap rowFunc . rowCluster &&&
        fmap colFunc . colCluster) cluOpt
       

-- |
plotColorBar :: (Renderable (Path R2) b, Renderable Text b,Backend b R2)
         => Para -> Diagram b R2
plotColorBar para =
    let step = (vMax - vMin) / (fromIntegral n)
        toD = map ((\c ->
                     rect w h # lcA transparent
                     # fc c
                   ) . chooseColor para)
        toText = printf "%.1f"
        tMin = toText vMin
        tMax = toText vMax
        tMean = toText vMean
    in case color of
        Two _ _ ->
            let ds = toD [vMin,vMin+step..vMax]
            in case colorBarPos para of
                Horizontal ->
                    let bar = hcat ds # centerXY ===
                              strutY (0.3*h)
                        toT t = text t
                                # font (fontName para)
                                # fontSize (0.95*h) <> strutY h      
                    in beside' (r2 (mW,-h)) (strutY (0.3*h) === toT tMax) $
                       beside' (r2 (-mW,-h)) (strutY (0.3*h) === toT tMin) (bar # centerY)
                Vertical ->
                    let bar = (rotate (90 @@ deg) $ hcat ds) # centerXY |||
                              strutX (0.3*h)
                        toT t = alignedText 0 0.5 t
                                # font (fontName para)
                                # fontSize (0.95*h) <> strutY h <>
                                rect (fromIntegral (length t) * h * 0.6) h
                                # lcA transparent # alignL
                    in beside' (r2 (h,mW)) (strutX (0.3*h) ||| toT tMax) $
                       beside' (r2 (h,-mW)) (strutX (0.3*h) ||| toT tMin) (bar # centerX)
        Three _ _ _ ->   
            let rhs = [vMean,vMean+step..vMax]
                lhs = reverse [vMean,vMean-step..vMin]
                ln = fromIntegral $ length lhs
                rn = fromIntegral $ length rhs
            in case colorBarPos para of
                Horizontal ->
                    let ds = hcat (toD lhs) # alignBR <>
                             hcat (toD rhs) # alignBL
                        bar = ds === strutY (0.3*h)
                        toT t = text t
                                # font (fontName para)
                                # fontSize (0.95*h) <> strutY h      
                    in beside' (r2 (mW*rn,-0.5*h)) (alignT $ strutY (0.3*h) === toT tMax) $
                       beside' (r2 (-mW*ln,-0.5*h)) (alignT $ strutY (0.3*h) === toT tMin) $
                       beside' unit_Y (toT tMean) bar
                Vertical ->
                    let ds = rotate (90 @@ deg) $
                             hcat (toD lhs) # alignBR <>
                             hcat (toD rhs) # alignBL
                        bar = ds |||
                              strutX (0.3*h)
                        toT t = alignedText 0 0.5 t
                                # font (fontName para)
                                # fontSize (0.95*h) <> strutY h <>
                                rect (fromIntegral (length t) * h * 0.6) h
                                # lcA transparent # alignL
                    in beside' (r2 (0.5*h,mW*rn)) (alignL $ strutX (0.3*h) ||| toT tMax) $
                       beside' (r2 (0.5*h,-mW*ln)) (alignL $ strutX (0.3*h) ||| toT tMin) $
                       beside' unitX (toT tMean) bar 
  where
    beside' v b a = beside v a b
    n = 200 :: Int
    ratio = 0.5
    mW = case colorBarPos para of
        Horizontal -> ratio * matrixWidth para
        Vertical -> ratio * matrixHeight para
    w = case colorBarPos para of
        Horizontal -> ratio * matrixWidth para / fromIntegral n
        Vertical -> ratio * matrixHeight para / fromIntegral n
    h = legendFontSize para
    color = colorOpt . clustOpt $ para
    ColorVal vMin vMean vMax = colorVal para

plotMatrix :: (Renderable (Path R2) b, Renderable Image b,Renderable Text b,
             Backend b R2)
           => Para -> Matrix -> Diagram b R2
plotMatrix para m =
    case tradeOff para of
        Performance -> plotMatrixPerformance para m
        Quality -> plotMatrixQuality para m
        
plotMatrixPerformance :: (Renderable (Path R2) b, Renderable Image b,
             Backend b R2) =>
            Para -> Matrix -> Diagram b R2
plotMatrixPerformance para m =
    image (plotMatrixCairo para m) (matrixWidth para) (matrixHeight para)
    

plotMatrixQuality :: (Renderable (Path R2) b, Renderable Text b,
             Backend b R2) =>
            Para -> Matrix -> Diagram b R2
plotMatrixQuality para m =
    let iCol = nCol m
        iRow = nRow m
        vec = dat m
        mH = matrixHeight para
        mW = matrixWidth para
        h = mH / fromIntegral iRow
        w = mW / fromIntegral iCol
        ls = [0..UV.length vec - 1]
    in case order m of
        RowMajor ->
            centerXY $ vcat $ map hcat $ chunksOf iCol $
            map (\idx ->
                  let v = vec `atUV` idx
                      c = chooseColor para v
                  in rect w h # lcA transparent # fc c) ls
        ColumnMajor ->
            centerXY $ hcat $ map vcat $ chunksOf iRow $
            map (\idx ->
                  let v = vec `atUV` idx
                      c = chooseColor para v
                  in rect w h # lcA transparent # fc c) ls


mkLabels :: (Backend b R2,Renderable (Path R2) b,Renderable Text b)
         => Bool
         -> Double
         -> Double
         -> String
         -> Maybe (V.Vector T.Text)
         -> [Diagram b R2]
mkLabels isLeft sizeFont maxSize fName textVec =
    let wVsH = 0.7
        size = if sizeFont > maxSize
               then maxSize
               else sizeFont
    in case textVec of
        Nothing -> []
        Just tVec ->
            map ((\str ->
                   if isLeft
                   then alignedText 0 0.5 str
                        # font fName
                        # fontSize (0.95 * size) <>
                        strutY size <>
                        rect ((fromIntegral $ length str) * size * wVsH) maxSize
                        # lcA transparent
                        # alignL
                   else alignedText 1 0.5 str
                        # font fName
                        # fontSize (0.95 * size) <>
                        strutY size <>
                        rect ((fromIntegral $ length str) * size * wVsH) maxSize
                        # lcA transparent
                        # alignR
                 ) . T.unpack) $ V.toList tVec
    
mkGroupLegend :: (Renderable (Path R2) b,Renderable Text b,Backend b R2)
              => Pos
              -> Double
              -> Double
              -> String
              -> HashMap T.Text (Colour Double)
              -> [(Diagram b R2,Diagram b R2)]
mkGroupLegend p w h f hash =
    let wVsH = 0.7
    in map (\(t,c) ->
             let r = centerX $ rect w h
                     # lcA transparent
                     # fc c ||| strutX h
                 d = case p of
                     Horizontal ->
                         centerXY $
                         alignedText 1 0.5 (T.unpack t)
                         # font f
                         # fontSize (0.95 * h) <> strutY h <>
                         rect (fromIntegral (T.length t) * h * wVsH) h
                         # lcA transparent
                         # alignR
                     Vertical ->
                         centerXY $
                         alignedText 0 0.5 (T.unpack t)
                         # font f
                         # fontSize (0.95 * h) <> strutY h <>
                         rect (fromIntegral (T.length t) * h * wVsH) h
                         # lcA transparent
                         # alignL
             in (r,d)) $ sortBy (compare `on` fst) $ H.toList hash
