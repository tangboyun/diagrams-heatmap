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

module Diagrams.HeatMap.Type
       ( MatrixOrder(..)
       , DisFunc
       , Matrix(..)
       , Dataset(..)
       , ColorOpt(..)
       , ColorVal(..)
       , ClustOpt(..)
       , Pos(..)
       , HPos(..)
       , VPos(..)
       , TradeOff(..)
       , Para(..)
       , Linkage(..)
       ) where

import           Data.Clustering.Hierarchical
import           Data.Colour
import           Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

data MatrixOrder = RowMajor
                 | ColumnMajor
                 deriving (Show,Eq)
                          
type DisFunc = UV.Vector Double -> UV.Vector Double -> Double

data Matrix = Matrix
  { nRow :: Int
  , nCol :: Int
  , order :: MatrixOrder
  , dat :: UV.Vector Double
  } deriving (Show,Eq)
    
data Dataset = Dataset 
  { rowNames :: Maybe (V.Vector Text)
  , colNames :: Maybe (V.Vector Text)
  , colLabels :: Maybe (V.Vector Text)
  , datM :: Matrix
  } deriving (Show,Eq)

data ColorOpt = Two { low :: Colour Double
                    , high :: Colour Double}
              | Three { low :: Colour Double
                      , medium :: Colour Double
                      , high :: Colour Double}
                deriving (Show,Eq)

data ColorVal = ColorVal
  { lowVal :: Double
  , mediumVal :: Double
  , highVal :: Double
  } deriving (Show,Eq)

data Pos = Horizontal
         | Vertical
         deriving (Show,Eq)
                  
data HPos = LeftTree
          | RightTree
          deriving (Show,Eq)
data VPos = TopTree
          | BottomTree
          deriving (Show,Eq)
                   
data TradeOff = Quality -- ^ 
              | Performance -- ^ For large matrix (i.e > 60000 elements), using raw cairo api and unsafePerformIO. 
              deriving (Show,Eq)

data ClustOpt = ClustOpt
  { colorOpt :: ColorOpt
  , rowCluster :: Maybe (DisFunc,Linkage,HPos)
  , colCluster :: Maybe (DisFunc,Linkage,VPos)
  }

data Para = Para
  { clustOpt :: ClustOpt
  , colorVal :: ColorVal
  , matrixHeight :: Double
  , matrixWidth :: Double
  , rowTreeHeight :: Double
  , rowTreeLineWidth :: Double
  , colTreeHeight :: Double
  , colTreeLineWidth :: Double
  , rowFontSize :: Double
  , colFontSize :: Double
  , legendFontSize :: Double
  , fontName :: String
  , colorBarPos :: Pos
  , tradeOff :: TradeOff
  }
