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


import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Diagrams.HeatMap.Impl
import           Diagrams.HeatMap.Internal
import           Diagrams.HeatMap.Module
import           Diagrams.HeatMap.Type


plotHeatMap :: (Renderable (Path R2) b,Renderable Text b,Renderable Image b,Backend b R2)
          => Para -> Dataset -> (Diagram b R2,Dataset)
plotHeatMap para dataset =
    let rowTreeLineW = rowTreeLineWidth para
        colTreeLineW = colTreeLineWidth para
        gapRatioForRowLabel = 0.02
        groupBarRatio = 0.5 * (sqrt 5 - 1)
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
        w = matrixWidth para / fromIntegral j
        h = matrixHeight para / fromIntegral i
        (rowDendro,colDendro) = clustering (clustOpt para) (datM dataset)
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
            let ali =
                    case rowTreePos of
                        LeftTree -> True
                        RightTree -> False
                dia = beside (rotate (Deg 180) rowTreeV)
                      (strutX $ gapRatioForRowLabel * matrixWidth para) $
                      centerY $ vcat $
                      mkLabels ali (rowFontSize para) h
                      (fontName para) (rowNames newDataset)
            in (dia,rotate (Deg 180) rowTreeV)
        (colLabelD,colLabelV) =
            let gW = w
                gH = groupBarRatio * gW
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
                               mkLabels ali (colFontSize para) w
                               (fontName para) (colNames newDataset)
                      in case colLabels newDataset of
                          Just _ ->
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
                  else let (dia,treeH) = toTree rowTreeLineW h  $ fromJust rowDendro
                           tree = scaleY (rowTreeHeight para / treeH) dia
                       in case rowTreePos of
                           LeftTree ->
                               transform (rotation $ Deg 90) $
                               transform reflectionX tree
                           RightTree -> transform (rotation $ Deg $ -90) tree
        colTree = if isNothing colDendro
                  then mempty
                  else let (dia,treeH) = toTree colTreeLineW w $ fromJust colDendro
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
        colorBar = plotColorBar para
    in case colorBarPos para of
        Horizontal ->
            let sep' = groupBarRatio * w * 0.5
                gD = centerXY $ hcat' (CatOpts Cat sep' Proxy) $
                     map (\(r,t) -> rotate (Deg 90) $ alignR $ t ||| strutX (2*sep') ||| r) legends
            in (centerXY $ (heatPlot === (gD # alignT # centerX ||| strutX (0.1 * matrixWidth para) ||| colorBar # alignT) # centerXY),newDataset)
        Vertical ->
            let sep' = groupBarRatio * w * 0.5
                gD = centerXY $ vcat' (CatOpts Cat sep' Proxy) $
                     map (\(r,t) -> alignL $ r ||| strutX (2*sep') ||| t) legends
            in (centerXY (heatPlot ||| (gD # alignL # centerY === strutY (0.1 * matrixHeight para) === colorBar # alignL) # centerXY),newDataset)


