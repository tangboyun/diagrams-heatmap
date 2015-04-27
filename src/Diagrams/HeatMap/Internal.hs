{-# LANGUAGE FlexibleContexts,TypeFamilies #-}
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
import           Diagrams.HeatMap.Dendrogram 
import           Diagrams.HeatMap.Impl
import           Diagrams.HeatMap.Module
import           Diagrams.HeatMap.Type
import           Diagrams.HeatMap.Unsafe


toTree :: (Renderable (Path V2 Double) b,Backend b V2 Double)
       => Double
       -> Double
       -> Dendrogram a
       -> (QDiagram b V2 Double Any,Double)
toTree lineW elemWidth dendro =
    let (path,wid) = first (dendrogramPath . fmap snd) $
                     fixedWidth elemWidth dendro
        dia = stroke path # lwG lineW # lc black # centerXY ===
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
       

mkColorBar :: (Renderable (Path V2 Double) b, Renderable (Text Double) b,Backend b V2 Double)
           => (Double,Double)
           -> ColorOpt
           -> ColorVal
           -> Pos
           -> String
           -> QDiagram b V2 Double Any
mkColorBar (w,h) color (ColorVal vMin vMean vMax) pos fName =
    let toText = printf "%.1f"
        tMin = toText vMin
        tMax = toText vMax
        tMean = toText vMean
        fontH = 0.5 * h
        toColorbar sts = rect w h # fillTexture (mkLinearGradient sts ((-0.5*w) ^& 0) (0.5*w ^& h) GradPad)
    in case color of
        Two lC hC ->
            let stops = mkStops [(lC,0,1),(hC,1,1)]
            in case pos of
                Horizontal ->
                    let bar = toColorbar stops 
                        toT t = text t
                                # font fName
                                # fontSizeL (0.95*fontH) <> strutY fontH      
                    in bar <>
                       position [ (p2 (-0.5*w,-0.5*h), alignT $ strutY (0.25*h) === toT tMin)
                                , (p2 (0.5*w,-0.5*h),  alignT $ strutY (0.25*h) === toT tMax)
                                ] 
                Vertical ->
                    let bar = (rotate (90 @@ deg) $ toColorbar stops) # centerXY
                        toT t = alignedText 0 0.5 t
                                # font fName
                                # fontSizeL (0.95*fontH) <> 
                                rect (fromIntegral (length t) * fontH * 0.6) fontH
                                # lcA transparent # alignL
                    in bar <>
                       position [ (p2 (0.5*h,0.5*w),  alignL $ strutX (0.25*h) ||| toT tMax)
                                , (p2 (0.5*h,-0.5*w), alignL $ strutX (0.25*h) ||| toT tMin)
                                ] 
        Three lC mC hC ->   
            let stops = mkStops [(lC,0,1),(mC,r,1),(hC,1,1)]
                r = (vMean-vMin)/(vMax-vMin)
            in case pos of
                Horizontal ->
                    let bar = toColorbar stops
                        toT t = text t
                                # font fName
                                # fontSizeL (0.95*fontH) <> strutY fontH      
                    in bar <>
                       position [ (p2 (-0.5*w,-0.5*h),    alignT $ strutY (0.25*h) === toT tMin)
                                , (p2 (w*(r-0.5),-0.5*h), alignT $ strutY (0.25*h) === toT tMean)
                                , (p2 (0.5*w,-0.5*h),     alignT $ strutY (0.25*h) === toT tMax)
                                ] 
                Vertical ->
                    let bar = rotate (90 @@ deg) $ toColorbar stops
                        toT t = alignedText 0 0.5 t
                                # font fName
                                # fontSizeL (0.95*fontH) <> 
                                rect (fromIntegral (length t) * fontH * 0.6) fontH
                                # lcA transparent # alignL
                    in bar <>
                       position [ (p2 (0.5*h,0.5*w), alignL $ strutX (0.25*h) ||| toT tMax)
                                , (p2 (0.5*h,(r-0.5)*w), alignL $ strutX (0.25*h) ||| toT tMean)
                                , (p2 (0.5*h,-0.5*w), alignL $ strutX (0.25*h) ||| toT tMin)
                                ] 

-- |
plotColorBar :: (Renderable (Path V2 Double) b, Renderable (Text Double) b,Backend b V2 Double)
         => Para -> QDiagram b V2 Double Any
plotColorBar p = mkColorBar (w,h) color (colorVal p) pos fName
  where
    ratio = 0.5
    pos = colorBarPos p
    fName = fontName p
    w = case colorBarPos p of
        Horizontal -> ratio * matrixWidth p
        Vertical -> ratio * matrixHeight p 
    h = legendFontSize p
    color = colorOpt . clustOpt $ p

plotMatrix :: (Renderable (Path V2 Double) b, Renderable (DImage Double External) b,Renderable (Text Double) b,
             Backend b V2 Double)
           => Para -> Matrix -> QDiagram b V2 Double Any
plotMatrix p m =
    case tradeOff p of
        Performance -> plotMatrixPerformance p m
        Quality -> plotMatrixQuality p m
        
plotMatrixPerformance :: (Renderable (Path V2 Double) b, Renderable (DImage Double External) b,
             Backend b V2 Double) =>
            Para -> Matrix -> QDiagram b V2 Double Any
plotMatrixPerformance p m =
    image $
    DImage
    (ImageRef (plotMatrixCairo p m))
    (round $ matrixWidth p)
    (round $ matrixHeight p)
    mempty
    

plotMatrixQuality :: (Renderable (Path V2 Double) b, Renderable (Text Double) b,
             Backend b V2 Double) =>
            Para -> Matrix -> QDiagram b V2 Double Any
plotMatrixQuality p m =
    let iCol = nCol m
        iRow = nRow m
        vec = dat m
        mH = matrixHeight p
        mW = matrixWidth p
        h = mH / fromIntegral iRow
        w = mW / fromIntegral iCol
        ls = [0..UV.length vec - 1]
    in case order m of
        RowMajor ->
            centerXY $ vcat $ map hcat $ chunksOf iCol $
            map (\idx ->
                  let v = vec `atUV` idx
                      c = chooseColor p v
                  in rect w h # lcA transparent # fc c) ls
        ColumnMajor ->
            centerXY $ hcat $ map vcat $ chunksOf iRow $
            map (\idx ->
                  let v = vec `atUV` idx
                      c = chooseColor p v
                  in rect w h # lcA transparent # fc c) ls


mkLabels :: (Backend b V2 Double,Renderable (Path V2 Double) b,Renderable (Text Double) b)
         => Bool
         -> Double
         -> Double
         -> String
         -> Maybe (V.Vector T.Text)
         -> [QDiagram b V2 Double Any]
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
                        # fontSizeL (0.95 * size) <>
                        strutY size <>
                        rect ((fromIntegral $ length str) * size * wVsH) maxSize
                        # lcA transparent
                        # alignL
                   else alignedText 1 0.5 str
                        # font fName
                        # fontSizeL (0.95 * size) <>
                        strutY size <>
                        rect ((fromIntegral $ length str) * size * wVsH) maxSize
                        # lcA transparent
                        # alignR
                 ) . T.unpack) $ V.toList tVec
    
mkGroupLegend :: (Renderable (Path V2 Double) b,Renderable (Text Double) b,Backend b V2 Double)
              => Pos
              -> Double
              -> Double
              -> String
              -> HashMap T.Text (Colour Double)
              -> [(QDiagram b V2 Double Any,QDiagram b V2 Double Any)]
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
                         # fontSizeL (0.95 * h) <> strutY h <>
                         rect (fromIntegral (T.length t) * h * wVsH) h
                         # lcA transparent
                         # alignR
                     Vertical ->
                         centerXY $
                         alignedText 0 0.5 (T.unpack t)
                         # font f
                         # fontSizeL (0.95 * h) <> strutY h <>
                         rect (fromIntegral (T.length t) * h * wVsH) h
                         # lcA transparent
                         # alignL
             in (r,d)) $ sortBy (compare `on` fst) $ H.toList hash
