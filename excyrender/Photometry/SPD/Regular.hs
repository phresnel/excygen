-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.SPD.Regular
( regularSPD
) where

import Photometry.SPD.SPD
import Photometry.CIEMatchingCurves
import Data.Vector as V


-- Regular -------------------------------------------------------------------------------------
regularSPD    :: (RealFrac t) => t -> t -> [t] -> SPD t



-- impl -------------------------------------------------------------------------------------------
regularSPD lambdaMin' lambdaMax' spectrum' =
    let delta' = (lambdaMax' - lambdaMin') /
                 (fromIntegral ((Prelude.length spectrum') - 1))
        inverseDelta = 1.0 / delta'
        sample' = sampleRegular lambdaMin' lambdaMax' spectrum' inverseDelta
    in SPD {
        sample  = sample',
        toXYZ   = toXYZRegular lambdaMin' inverseDelta sample',
        stretch = \f -> regularSPD lambdaMin' lambdaMax' $ Prelude.map (f*) spectrum'
    }


sampleRegular lambdaMin lambdaMax spectrum inverseDelta lambda
    | lambda < lambdaMin = 0
    | lambda > lambdaMax = 0
    | otherwise = let
        x = (lambda - lambdaMin) * inverseDelta
        b0 = floor x
        b1 = min (b0+1) ((Prelude.length spectrum) - 1)
        dx = x - fromIntegral b0
      in (1.0 - dx) * spectrum!!b0 + dx * spectrum!!b1


toXYZRegular :: (RealFrac t, Num t) => t -> t -> (t->t) -> (t,t,t)
toXYZRegular lambdaMin inverseDelta sample =  
    let samples = V.map f (V.enumFromN 0 cie_length)
                   where f i = sample $ lambdaMin + inverseDelta * fromIntegral i 
    in (cie_inverse_length * V.sum (V.zipWith (*) cie_x' samples),
        cie_inverse_length * V.sum (V.zipWith (*) cie_y' samples),
        cie_inverse_length * V.sum (V.zipWith (*) cie_z' samples))

