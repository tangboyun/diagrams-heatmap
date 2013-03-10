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

module Diagrams.HeatMap.Type where

import           Data.Clustering.Hierarchical
import           Data.Colour
import           Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

data MatrixOrder = RowMajor | ColumnMajor deriving (Show,Eq)
type DisFunc = UV.Vector Double -> UV.Vector Double -> Double

data Matrix = Matrix
  { nRow :: {-# UNPACK #-} !Int
  , nCol :: {-# UNPACK #-} !Int
  , order :: !MatrixOrder
  , dat :: !(UV.Vector Double)
  } deriving (Show,Eq)
    
data Dataset = Dataset 
  { rowNames :: !(Maybe (V.Vector Text))
  , colNames :: !(Maybe (V.Vector Text))
  , colLabels :: !(Maybe (V.Vector Text))
  , datM :: !Matrix
  } deriving (Show,Eq)

data ColorOpt = Two { low :: Colour Double
                    , high :: Colour Double}
              | Three { low :: Colour Double
                      , medium :: Colour Double
                      , high :: Colour Double}
                deriving (Show,Eq)

data ColorVal = ColorVal
  { lowVal :: {-# UNPACK #-} !Double
  , mediumVal :: {-# UNPACK #-} !Double
  , highVal :: {-# UNPACK #-} !Double
  } deriving (Show,Eq)

data Pos = Horizontal | Vertical deriving (Show,Eq)
data HPos = LeftTree | RightTree deriving (Show,Eq)
data VPos = TopTree | BottomTree deriving (Show,Eq)
data TradeOff = Quality | Performance deriving (Show,Eq)

data ClustOpt = ClustOpt
  { colorOpt :: ColorOpt
  , rowCluster :: Maybe (DisFunc,Linkage,HPos)
  , colCluster :: Maybe (DisFunc,Linkage,VPos)
  } 

data Para = Para
  { clustOpt :: !ClustOpt
  , colorVal :: !ColorVal
  , matrixHeight :: {-# UNPACK #-} !Double
  , matrixWidth :: {-# UNPACK #-} !Double
  , rowTreeHeight :: {-# UNPACK #-} !Double
  , colTreeHeight :: {-# UNPACK #-} !Double
  , rowFontSize :: {-# UNPACK #-} !Double
  , colFontSize :: {-# UNPACK #-} !Double
  , fontName :: !String
  , colorBarPos :: !Pos
  , tradeOff :: ! TradeOff
  } 
