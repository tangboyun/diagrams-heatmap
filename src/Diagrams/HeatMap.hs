{-# LANGUAGE FlexibleContexts,OverloadedStrings,BangPatterns #-}
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
module Diagrams.HeatMap where

import           Control.Arrow hiding ((|||))
import           Data.Clustering.Hierarchical
import           Data.Colour.Names
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Colour.SRGB
import           Data.Colour.SRGB.Linear
import           Data.Foldable hiding (concatMap)
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.RealFloat
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Diagrams.Dendrogram as D
import           Diagrams.HeatMap.Impl
import           Diagrams.HeatMap.Type
import           Diagrams.HeatMap.Unsafe
import           Diagrams.Prelude hiding (trace)
import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Text
import           Statistics.Sample
import           Text.Printf

mkColorBar :: (Renderable (Path R2) b, Renderable Text b,Backend b R2)
         => Para -> Diagram b R2
mkColorBar para =
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
        Two lC hC ->
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
                    let bar = (rotate (Deg 90) $ hcat ds) # centerXY |||
                              strutX (0.3*h)
                        toT t = alignedText 0 0.5 t
                                # font (fontName para)
                                # fontSize (0.95*h) <> strutY h <>
                                rect (fromIntegral (length t) * h * 0.6) h
                                # lcA transparent # alignL
                    in beside' (r2 (h,mW)) (strutX (0.3*h) ||| toT tMax) $
                       beside' (r2 (h,-mW)) (strutX (0.3*h) ||| toT tMin) (bar # centerX)
        Three lC mC hC ->   
            let rhs = [vMean,vMean+step..vMax]
                lhs = reverse [vMean,vMean-step..vMin]
            in case colorBarPos para of
                Horizontal ->
                    let ds = hcat (toD lhs) # alignBR <>
                             hcat (toD rhs) # alignBL
                        bar = ds === 
                              strutY (0.3*h)
                        toT t = text t
                                # font (fontName para)
                                # fontSize (0.95*h) <> strutY h      
                    in beside' (r2 (mW,-h)) (strutY (0.3*h) === toT tMax) $
                       beside' (r2 (-mW,-h)) (strutY (0.3*h) === toT tMin) $
                       beside' unit_Y (toT tMean) (bar # centerY)
                Vertical ->
                    let ds = rotate (Deg 90) $
                             hcat (toD lhs) # alignBR <>
                             hcat (toD rhs) # alignBL
                        bar = ds |||
                              strutX (0.3*h)
                        toT t = alignedText 0 0.5 t
                                # font (fontName para)
                                # fontSize (0.95*h) <> strutY h <>
                                rect (fromIntegral (length t) * h * 0.6) h
                                # lcA transparent # alignL
                    in beside' (r2 (h,mW)) (strutX (0.3*h) ||| toT tMax) $
                       beside' (r2 (h,-mW)) (strutX (0.3*h) ||| toT tMin) $
                       beside' unitX (toT tMean) (bar # centerX)
  where
    beside' v b a = beside v a b
    n = 200
    ratio = 0.5
    barHWRatio = 0.05
    mW = case colorBarPos para of
        Horizontal -> ratio * matrixWidth para
        Vertical -> ratio * matrixHeight para
    w = case colorBarPos para of
        Horizontal -> ratio * matrixWidth para / fromIntegral n
        Vertical -> ratio * matrixHeight para / fromIntegral n
    h = barHWRatio * w * fromIntegral n
    color = colorOpt . clustOpt $ para
    ColorVal vMin vMean vMax = colorVal para


mkLabels :: (Backend b R2,Renderable (Path R2) b,Renderable Text b) => Bool -> Double -> Double -> String -> Maybe (V.Vector T.Text) -> [Diagram b R2]
mkLabels isLeft sizeFont maxSize fontName textVec =
    let wVsH = 0.65
        size = if sizeFont > maxSize
               then maxSize
               else sizeFont
    in case textVec of
        Nothing -> []
        Just tVec ->
            map ((\str ->
                   if isLeft
                   then alignedText 0 0.5 str
                        # font fontName
                        # fontSize (0.95 * size) <>
                        strutY size <>
                        rect ((fromIntegral $ length str) * size * wVsH) maxSize
                        # lcA transparent
                        # alignL
                   else alignedText 1 0.5 str
                        # font fontName
                        # fontSize (0.95 * size) <>
                        strutY size <>
                        rect ((fromIntegral $ length str) * size * wVsH) maxSize
                        # lcA transparent
                        # alignR
                 ) . T.unpack) $ V.toList tVec
    

toTree :: (Renderable (Path R2) b,Backend b R2) => Double -> Double -> Dendrogram a -> (Diagram b R2,Double)
toTree lineWidth elemWidth dendro =
    let (path,width) = first (D.dendrogramPath . fmap snd) $ D.fixedWidth elemWidth dendro
        dia = stroke path # lw lineWidth # centerXY === strutX width
        h = case dendro of
            Leaf _ -> 0
            Branch d _ _ -> d
    in (dia,h)


mkGroupLegend :: (Renderable (Path R2) b,Renderable Text b,Backend b R2) => Pos -> Double -> Double -> String -> HashMap T.Text (Colour Double) -> [(Diagram b R2,Diagram b R2)]
mkGroupLegend p w h f hash =
    let ts = sortBy (compare `on` fst) $ H.toList hash
        wVsH = 0.65
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
             in (r,d)) ts

            
clustering :: ClustOpt -> Matrix
           -> (Maybe (Dendrogram Int)  -- row dendro
             ,Maybe (Dendrogram Int)) -- col dendro
clustering cluOpt m =
    let rowClu = rowCluster cluOpt
        colClu = colCluster cluOpt
        vecOrd = order m
        i = nRow m
        j = nCol m
        colDendro =
            case colClu of
                Nothing -> Nothing
                Just (disFuncC,linkageC,_) ->
                    let v = case vecOrd of
                                 ColumnMajor -> dat m
                                 RowMajor -> dat $ changeOrder m
                        vvecC = V.fromList $
                                map (\idx -> UV.slice (idx*i) i v) [0..j-1]
                    in Just $
                       dendrogram linkageC [0..j-1]
                       (disFuncC `on` (vvecC `atV`))
        rowDendro =
            case rowClu of
                Nothing -> Nothing
                Just (disFuncR,linkageR,_) ->
                    let v = case vecOrd of
                                 ColumnMajor -> dat $ changeOrder m
                                 RowMajor -> dat m
                        vvecR = V.fromList $
                                map (\idx -> UV.slice (idx*j) j v) [0..i-1]
                    in Just $
                       dendrogram linkageR [0..i-1]
                       (disFuncR `on` (vvecR `atV`))
    in (rowDendro,colDendro)

toHeatMap :: (Renderable (Path R2) b,Renderable Text b,Renderable Image b,Backend b R2)
          => Para -> Dataset -> (Diagram b R2,Dataset)
toHeatMap para dataset =
    let lineWidth = 0.01
        gapRatioForRowLabel = 0.02
        gapRatioForColLabel = 0.05
        groupBarWidthRatio = 0.05
        groupBarRatio = 0.5 * (sqrt 5 - 1)
        labelHash = case colLabels dataset of
            Nothing -> H.empty
            Just tVec ->
                let uniqeGV = nub $ sort $ V.toList tVec
                    nGroup = length uniqeGV
                    (h,s,l) = hslView $ toRGB red
                in H.fromList $
                   zip uniqeGV $
                   map
                   (uncurryRGB sRGB .
                    (\newH -> hsl newH s l).
                    (h+).(*(360 / (fromIntegral nGroup))) . fromIntegral
                   ) [0..nGroup-1]
        matrix = datM dataset
        i = nRow matrix
        j = nCol matrix
        w = matrixWidth para / fromIntegral j
        h = matrixHeight para / fromIntegral i
        (rowDendro,colDendro) = clustering (clustOpt para) (datM dataset)
        rowIdxVec = fromMaybe (V.enumFromN 0 i) $ fmap (V.fromList . toList) $ rowDendro
        colIdxVec = fromMaybe (V.enumFromN 0 j) $ fmap (V.fromList . toList) $ colDendro
        newMatrix = let v = dat matrix
                    in matrix {dat = UV.generate (UV.length v)
                                     (\idx ->
                                       let (r,c) = idx `divMod` j
                                           r' = rowIdxVec `atV` r
                                           c' = colIdxVec `atV` c
                                           idx' = r'*j+c'
                                       in v `atUV` idx')
                              }
        newDataset = let rNVec = rowNames dataset
                         cNVec = colNames dataset
                         cLVec = colLabels dataset
                     in
                      Dataset { rowNames = fmap (flip V.backpermute rowIdxVec) rNVec
                              , colNames = fmap (flip V.backpermute colIdxVec) cNVec
                              , colLabels = fmap (flip V.backpermute colIdxVec) cLVec
                              , datM = newMatrix
                              }
        (rowTreePos,rowTreeV) =
            case rowCluster $ clustOpt para of
                Nothing -> (LeftTree,unit_X)
                Just (_,_,p) -> case p of
                    LeftTree -> (LeftTree,unit_X)
                    RightTree -> (RightTree,unitX)
        (colTreePos,colTreeV) =
            case colCluster $ clustOpt para of
                Nothing -> (TopTree,unitY)
                Just (_,_,p) -> case p of
                    TopTree -> (TopTree,unitY)
                    BottomTree -> (BottomTree,unit_Y)
        matrixD = plotMatrix para newMatrix
        (rowLabelD,rowLabelV) =
            let align =
                    case rowTreePos of
                        LeftTree -> True
                        RightTree -> False
                dia = beside (rotate (Deg 180) rowTreeV)
                      (strutX $ gapRatioForRowLabel * matrixWidth para) $
                      centerY $ vcat $
                      mkLabels align (rowFontSize para) h
                      (fontName para) (rowNames newDataset)
            in (dia,rotate (Deg 180) rowTreeV)
        (colLabelD,colLabelV) =
            let gW = w
                gH = groupBarRatio * gW
                align =
                    case colTreePos of
                        TopTree -> False
                        BottomTree -> True
                rs = 
                    case colLabels newDataset of
                        Nothing -> []
                        Just lVec ->
                            map
                            ((\c ->
                               centerY $
                               beside colTreeV
                               (strutY gH) $
                               beside (rotate (Deg 180) colTreeV)
                               (strutY (2*gH)) $
                               rect gW gH
                               # lcA transparent
                               # fc c
                             ) . (labelHash H.!)) $ V.toList lVec
                dia = let ds = map (
                                case colTreePos of
                                    TopTree -> rotate (Deg 90)
                                    BottomTree -> rotate (Deg 90)) $
                               mkLabels align (colFontSize para) w
                               (fontName para) (colNames newDataset)
                      in case colLabels newDataset of
                          Just lVec ->
                              centerXY $ hcat $
                              zipWith
                              (beside' colTreeV) rs ds
                          Nothing -> centerXY $
                                     case colTreePos of
                                          TopTree -> strutY (2*gH) === hcat ds
                                          BottomTree -> hcat ds === strutY (2*gH)
            in (dia,rotate (Deg 180) colTreeV)
        rowTree = if isNothing rowDendro
                  then mempty
                  else let (dia,treeH) = toTree lineWidth h  $ fromJust rowDendro
                           tree = scaleY (rowTreeHeight para / treeH) dia
                       in case rowTreePos of
                           LeftTree ->
                               transform (rotation $ Deg 90) $
                               transform reflectionX tree
                           RightTree -> transform (rotation $ Deg $ -90) tree
        colTree = if isNothing colDendro
                  then mempty
                  else let (dia,treeH) = toTree lineWidth w $ fromJust colDendro
                           tree = scaleY (colTreeHeight para / treeH) dia
                       in case colTreePos of
                           TopTree -> tree
                           BottomTree -> transform reflectionY tree
        legends = let gW = w
                      gH = groupBarRatio * gW
                  in mkGroupLegend (colorBarPos para) gW gH (fontName para) labelHash
        beside' v a b = beside v b a
        heatPlot = beside' rowLabelV rowLabelD $
                   beside' colLabelV colLabelD $
                   beside' colTreeV colTree $
                   beside' rowTreeV rowTree matrixD
        colorBar = mkColorBar para
    in 
     case colorBarPos para of
        Horizontal ->
            let sep = groupBarRatio * w * 0.5
                gD = centerXY $ hcat' (CatOpts Cat sep Proxy) $
                     map (\(r,t) -> rotate (Deg 90) $ alignR $ t ||| strutX (2*sep) ||| r) legends
            in (centerXY $ (heatPlot === (gD # alignT # centerX ||| strutX (0.1 * matrixWidth para) ||| colorBar # alignT) # centerXY),newDataset)
        Vertical ->
            let sep = groupBarRatio * w * 0.5
                gD = centerXY $ vcat' (CatOpts Cat sep Proxy) $
                     map (\(r,t) -> alignL $ r ||| strutX (2*sep) ||| t) legends
            in (centerXY (heatPlot ||| (gD # alignL # centerY === strutY (0.1 * matrixHeight para) === colorBar # alignL) # centerXY),newDataset)



renderDataset :: T.Text -> Dataset -> TL.Text
renderDataset sep dataset =
    let firstLine = tVecToB (fromText sep) 0
    in toLazyText $ go firstLine 0
  where
     i = nRow . datM $ dataset
     j = nCol . datM $ dataset
     (<>) = mappend
     cNVec = fromMaybe (V.generate j (T.pack . show)) $
             colNames dataset
     rNVec = fromMaybe (V.generate i (T.pack . show)) $
             rowNames dataset
     vec =
         case order $ datM dataset of
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

-- | pearson correlation coef
pcc :: UV.Vector Double -> UV.Vector Double -> Double
pcc v1 v2 =
  let m1 = mean v1
      m2 = mean v2
  in (\(num,(res1,res2)) ->
       num / (sqrt $ res1 * res2)
       ) $
     UV.ifoldl'
     (\(num,(res1,res2)) i e1 ->
       let val1 = e1
           val2 = UV.unsafeIndex v2 i
           num' = num + (val1-m1) * (val2-m2)
           res1' = res1 + (val1-m1)^2
           res2' = res2 + (val2-m2)^2
       in (num',(res1',res2'))
     ) (0,(0,0)) v1
{-# INLINE pcc #-}

eucDis :: UV.Vector Double -> UV.Vector Double -> Double
eucDis v1 v2 = 
    sqrt $ UV.foldl1' (+) $ UV.map (^2) $ UV.zipWith (-) v1 v2
{-# INLINE eucDis #-}



plotMatrix :: (Renderable (Path R2) b, Renderable Image b,Renderable Text b,
             Backend b R2) =>
            Para -> Matrix -> Diagram b R2
plotMatrix para m =
    case tradeOff para of
        Performance -> plotMatrixPerformance para m
        Quality -> plotMatrixQuality para m
        
plotMatrixPerformance :: (Renderable (Path R2) b, Renderable Image b,
             Backend b R2) =>
            Para -> Matrix -> Diagram b R2
plotMatrixPerformance para m = image (plotMatrixCairo para m) (matrixWidth para) (matrixHeight para)
    

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
        n = iCol * iRow
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
        -- RowMajor ->
        --     centerXY $ vcat $ map
        --     (\i ->
        --       hcat $
        --       map ((\c ->
        --              rect w h # lcA transparent # fc c) .
        --            chooseColor para) $
        --       UV.toList $ UV.slice (i*iCol) iCol vec) [0..iRow-1]
        -- ColumnMajor ->
        --     centerXY $ hcat $ map
        --     (\i ->
        --       vcat $
        --       map ((\c -> rect w h # lcA transparent # fc c) .
        --            chooseColor para) $
        --       UV.toList $ UV.slice (i*iRow) iRow vec) [0..iCol-1]
