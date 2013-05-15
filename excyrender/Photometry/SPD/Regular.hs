-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.SPD.Regular
( regularSPD
) where

import qualified Photometry.SPD.SPD as SPD
import Photometry.CIEMatchingCurves
import Data.Vector.Unboxed as V
import RealNum


-- Regular -------------------------------------------------------------------------------------
regularSPD    :: RealNum -> RealNum -> [RealNum] -> SPD.SPD



-- impl -------------------------------------------------------------------------------------------
regularSPD lambdaMin' lambdaMax' spectrum'' =
    let spectrum' = V.fromList spectrum''
        delta' = (lambdaMax' - lambdaMin') /
                 (fromIntegral ((V.length spectrum') - 1))
        inverseDelta = 1.0 / delta'
        sample' = sampleRegular lambdaMin' lambdaMax' spectrum' inverseDelta
    in SPD.SPD {
        SPD.sample  = sample',
        SPD.toXYZ   = toXYZRegular lambdaMin' spectrum' inverseDelta,
        SPD.stretch = \f -> regularSPD lambdaMin' lambdaMax' $ V.toList $ V.map (f*) spectrum'
    }


sampleRegular :: RealNum -> RealNum -> V.Vector RealNum -> RealNum -> RealNum -> RealNum
sampleRegular lambdaMin lambdaMax spectrum inverseDelta lambda
    | lambda < lambdaMin = 0
    | lambda > lambdaMax = 0
    | otherwise = sampleRegular_unchecked lambdaMin spectrum inverseDelta lambda


sampleRegular_unchecked :: RealNum -> V.Vector RealNum -> RealNum -> RealNum -> RealNum
sampleRegular_unchecked lambdaMin spectrum inverseDelta lambda =
    let
      x = (lambda - lambdaMin) * inverseDelta
      b0 = floor x
      b1 = min (b0+1) ((V.length spectrum) - 1)
      dx = x - fromIntegral b0
    in (1.0 - dx) * spectrum!b0 + dx * spectrum!b1


toXYZRegular :: RealNum -> V.Vector RealNum -> RealNum -> (RealNum,RealNum,RealNum)
toXYZRegular lambdaMin spectrum inverseDelta =  
    let samples = V.map f (V.enumFromN 0 cie_length)
                   where f i = sampleRegular_unchecked lambdaMin spectrum inverseDelta 
                                                       $ lambdaMin + inverseDelta * i 
    in (cie_inverse_length * V.sum (V.zipWith (*) cie_x' samples),
        cie_inverse_length * V.sum (V.zipWith (*) cie_y' samples),
        cie_inverse_length * V.sum (V.zipWith (*) cie_z' samples))

