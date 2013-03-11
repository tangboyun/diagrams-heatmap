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

import Graphics.Rendering.Cairo hiding (Matrix)
import System.IO
import System.IO.Unsafe
import Diagrams.HeatMap.Type
import Diagrams.HeatMap.Impl
import qualified Data.Vector.Unboxed as UV
import Control.Monad
import System.Directory
import Data.Colour.RGBSpace
import Data.Colour.SRGB

-- matrix order
plotMatrixCairo :: Para -> Matrix -> FilePath
{-# NOINLINE plotMatrixCairo #-}
plotMatrixCairo para m =
    unsafePerformIO $
    withImageSurface FormatRGB24
     (round mW) (round mH) $ \surf -> do
         let ls = [0..UV.length v - 1]
             v = case order m of
                   RowMajor -> dat m
                   ColumnMajor -> dat $ changeOrder m
         renderWith surf $
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
      myRect x y = rectangle x y w h
      setRGB r g b = setSourceRGBA r g b 1
      setColor co = uncurryRGB setRGB (toSRGB co)