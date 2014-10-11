-----------------------------------------------------------------------------
-- |
-- Module :
-- Copyright : (c) 2014 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
--
--
-----------------------------------------------------------------------------
module Main where

import           Data.Colour.Names
import           Data.Colour.Palette.BrewerSet
import           Diagrams.Backend.Cairo
import           Diagrams.Combinators
import           Diagrams.HeatMap.Internal
import           Diagrams.HeatMap.Type
import           Diagrams.TwoD.Size
import           System.Environment


color1 = let cSet = brewerSet RdBu 11
        in Three (cSet !! 9) white (cSet !! 1)


color2 = let cSet = brewerSet Blues 9
        in Two white (cSet !! 8)

main :: IO ()
main = do
    fp:_ <- getArgs
    renderCairo fp (Width 100) $ pad 1.25 $
        mkColorBar (20,2) color2 (ColorVal 5 8 15) Vertical "Arail"

