{-# LANGUAGE FlexibleContexts,OverloadedStrings #-}
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
import           Data.Default.Class
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Diagrams.HeatMap.Impl
import           Diagrams.HeatMap.Internal
import           Diagrams.HeatMap.Module
import           Diagrams.HeatMap.Type

plotHeatMap :: (Renderable (Path V2 Double) b,Renderable (Text Double) b,Renderable (DImage Double External) b,Backend b V2 Double)
          => Para -> Dataset -> (QDiagram b V2 Double Any,Dataset)
plotHeatMap p dataset =
    let rowTreeLineW = rowTreeLineWidth p
        colTreeLineW = colTreeLineWidth p
        gapRatioForRowLabel = 0.02
        legendFS = legendFontSize p
        labelHash = case colLabels dataset of
            Nothing -> H.empty
            Just tVec ->
                let uniqeGV = nub $ sort $ V.toList tVec
                    nGroup = length uniqeGV
                    (hu,s,l) = hslView $ toSRGB red
                in H.fromList $
                   zip uniqeGV $
                   map
                   (uncurryRGB sRGB .
                    (\newH -> hsl newH s l).
                    (hu+).(*(360 / (fromIntegral nGroup))) . fromIntegral
                   ) [0..nGroup-1]
        matrix = datM dataset
        i = nRow matrix
        j = nCol matrix
        w = matrixWidth p / fromIntegral j
        h = matrixHeight p / fromIntegral i
        (rowDendro,colDendro) = clustering (clustOpt p) (datM dataset)
        rowIdxVec = fromMaybe (V.enumFromN 0 i) $ fmap (V.fromList . toList) $ rowDendro
        colIdxVec = fromMaybe (V.enumFromN 0 j) $ fmap (V.fromList . toList) $ colDendro
        newMatrix = let v = case order matrix of
                            RowMajor -> dat matrix
                            ColumnMajor -> dat $ changeOrder matrix
                    in matrix {dat = UV.generate (UV.length v)
                                     (\idx ->
                                       let (r,c) = idx `divMod` j
                                           r' = rowIdxVec `atV` r
                                           c' = colIdxVec `atV` c
                                           idx' = r'*j+c'
                                       in v `atUV` idx')
                              ,order = RowMajor      
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
            case rowCluster $ clustOpt p of
                Nothing -> (LeftTree,unit_X)
                Just (_,_,posi) -> case posi of
                    LeftTree -> (LeftTree,unit_X)
                    RightTree -> (RightTree,unitX)
        (colTreePos,colTreeV) =
            case colCluster $ clustOpt p of
                Nothing -> (TopTree,unitY)
                Just (_,_,posi) -> case posi of
                    TopTree -> (TopTree,unitY)
                    BottomTree -> (BottomTree,unit_Y)
        matrixD = plotMatrix p newMatrix
        (rowLabelD,rowLabelV) =
            let ali =
                    case rowTreePos of
                        LeftTree -> True
                        RightTree -> False
                dia = beside (rotate (180 @@ deg) rowTreeV)
                      (strutX $ gapRatioForRowLabel * matrixWidth p) $
                      centerY $ vcat $
                      mkLabels ali (rowFontSize p) h
                      (fontName p) (rowNames newDataset)
            in (dia,rotate (180 @@ deg) rowTreeV)
        (colLabelD,colLabelV) =
            let gW = w
                gH = 0.618 * gW
                ali =
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
                               beside (rotate (180 @@ deg) colTreeV)
                               (strutY (2*gH)) $
                               rect gW gH
                               # lcA transparent
                               # fc c
                             ) . (labelHash H.!)) $ V.toList lVec
                dia = let ds = map (
                                case colTreePos of
                                    TopTree -> rotate (90 @@ deg)
                                    BottomTree -> rotate (90 @@ deg)) $
                               mkLabels ali (colFontSize p) w
                               (fontName p) (colNames newDataset)
                      in case colLabels newDataset of
                          Just _ ->
                              centerXY $ hcat $
                              zipWith
                              (beside' colTreeV) rs ds
                          Nothing -> centerXY $
                                     case colTreePos of
                                          TopTree -> strutY (2*gH) === hcat ds
                                          BottomTree -> hcat ds === strutY (2*gH)
            in (dia,rotate (180 @@ deg) colTreeV)
        rowTree = if isNothing rowDendro
                  then mempty
                  else let (dia,treeH) = toTree rowTreeLineW h  $ fromJust rowDendro
                           tree = scaleY (rowTreeHeight p / treeH) dia
                       in case rowTreePos of
                           LeftTree ->
                               transform (rotation $ 90 @@ deg) $
                               transform reflectionX tree
                           RightTree -> transform (rotation $ (-90) @@ deg) tree
        colTree = if isNothing colDendro
                  then mempty
                  else let (dia,treeH) = toTree colTreeLineW w $ fromJust colDendro
                           tree = scaleY (colTreeHeight p / treeH) dia
                       in case colTreePos of
                           TopTree -> tree
                           BottomTree -> transform reflectionY tree
        legends = let gW = w
                      gH = legendFS
                  in mkGroupLegend (colorBarPos p) gW gH (fontName p) labelHash
        beside' v a b = beside v b a
        heatPlot = beside' rowLabelV rowLabelD $
                   beside' colLabelV colLabelD $
                   beside' colTreeV colTree $
                   beside' rowTreeV rowTree matrixD
        colorBar = plotColorBar p
        sep' = legendFS * 0.5
        catOptSetteing =  set sep sep' $ set catMethod Cat def
    in case colorBarPos p of
        Horizontal ->
            let gD = centerXY $ rotate ((-90) @@ deg) $ hcat' catOptSetteing $
                     map (\(r,t) -> rotate (90 @@ deg) $ alignR $ t ||| strutX (2*sep') ||| r) legends
            in (centerXY $ (heatPlot === strutY (2*sep') === (gD # alignT # centerX ||| strutX (0.1 * matrixWidth p) ||| colorBar # alignT) # centerXY),newDataset)
        Vertical ->
            let gD = centerXY $ vcat' catOptSetteing $
                     map (\(r,t) -> alignL $ r ||| strutX (2*sep') ||| t) legends
            in (centerXY (heatPlot ||| (gD # alignL # centerY === strutY (0.1 * matrixHeight p) === colorBar # alignL) # centerXY),newDataset)


