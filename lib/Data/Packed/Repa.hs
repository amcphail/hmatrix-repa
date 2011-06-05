-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Repa
-- Copyright   :  (c) Alexander Vivian Hugh McPhail 2011
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Repa / hmatrix conversion functions
--
-----------------------------------------------------------------------------

module Data.Packed.Repa (
                         vectorToRepa
                         , repaToVector
                         , matrixToRepa
                         , repaToMatrix
                         ) where

import qualified Data.Packed.Vector as HV
import qualified Data.Packed.Matrix as HM

import Foreign.Storable

import Data.Vector.Storable

import qualified Data.Array.Repa as RA

-- | convert a Storable vector to a DIM1 repa array
vectorToRepa :: (Storable a, RA.Elt a)
               => HV.Vector a
             -> RA.Array RA.DIM1 a
vectorToRepa v = let ln = HV.dim v
                 in RA.fromVector (RA.Z RA.:. ln) $ convert v

-- | convert a 1d repa array to a Storable vector
repaToVector :: (Storable a, RA.Elt a)
               => RA.Array RA.DIM1 a
                 -> HV.Vector a
repaToVector = convert . RA.toVector

-- | convert a Storable matrix to a DIM2 repa array
matrixToRepa :: (Storable a, HM.Element a, RA.Elt a)
               => HM.Matrix a
             -> RA.Array RA.DIM2 a
matrixToRepa m = let (r,c) = (HM.rows m,HM.cols m) 
                 in RA.fromVector (RA.Z RA.:. r RA.:. c) $ convert $ HM.flatten m

-- | convert a 2d repa array to a Storable matrix
repaToMatrix :: (Storable a, RA.Elt a)
               => RA.Array RA.DIM2 a
             -> HM.Matrix a
repaToMatrix a = let (RA.Z RA.:. r RA.:. c) = RA.extent a
                 in HM.reshape c $ convert $ RA.toVector a
