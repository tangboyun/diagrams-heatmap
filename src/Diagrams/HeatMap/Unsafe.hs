{-# LANGUAGE BangPatterns #-}
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

module Diagrams.HeatMap.Unsafe where

import           Control.Monad
import qualified Data.Vector.Unboxed      as UV
import           Diagrams.Attributes
import           Diagrams.HeatMap.Impl
import           Diagrams.HeatMap.Type
import           Graphics.Rendering.Cairo hiding (Matrix)
import           System.Directory
import           System.IO
import           System.IO.Unsafe

plotMatrixCairo :: Para -> Matrix -> FilePath
{-# NOINLINE plotMatrixCairo #-}
plotMatrixCairo para m =
    unsafePerformIO $
    withImageSurface FormatARGB32
     (round mW) (round mH) $ \surf -> do
         let ls = [0..UV.length v - 1]
             v = case order m of
                   RowMajor -> dat m
                   ColumnMajor -> dat $ changeOrder m
         renderWith surf $ do
             clearSurface
             forM_ ls $ \idx -> do
                 let (r,c) = idx `divMod` j
                     value = v `atUV` idx
                     color = chooseColor para value
                 setColor color
                 myRect (fromIntegral c * w) (fromIntegral r * h)
                 fill
         tmpDir <- getTemporaryDirectory
         (fp,handle) <- openBinaryTempFile tmpDir "tmpMatrix.png"
         hClose handle
         surfaceWriteToPNG surf fp
         return fp
    where
      w = mW / fromIntegral j
      h = mH / fromIntegral i
      mW = matrixWidth para
      mH = matrixHeight para
      i = nRow m
      j = nCol m
      myRect x1 y1 = rectangle x1 y1 w h
      clearSurface = do
          setSourceRGBA 0 0 0 0
          rectangle 0 0 mW mH
          fill
      setColor = (\(r,g,b,a) -> setSourceRGBA r g b a) . colorToSRGBA
