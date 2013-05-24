-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.SPD.Regular
( regularSPD,
  regularSPDFromRGB
) where

import qualified Photometry.SPD.SPD as SPD
import Photometry.CIEMatchingCurves
import Photometry.RGBToSpectrumCurves
import Data.Vector.Unboxed as V
import RealNum
import Photometry.RGB(RGB(..))


-- Regular -------------------------------------------------------------------------------------
regularSPD    :: RealNum -> RealNum -> [RealNum] -> SPD.SPD
regularSPDFromRGB :: RGB -> SPD.SPD



-- impl -------------------------------------------------------------------------------------------

-- This function was ported from PBRT, v2
regularSPDFromRGB (RGB r g b)
    | r<=g && r<=b = 
        -- Compute reflectance _SampledSpectrum_ with _r_ as minimum
        let u     = V.map (r*) rgbRefl2SpectWhite'             -- r += r * rgbRefl2SpectWhite
            (v,w) = if g<=b
                    then (V.map ((g-r)*) rgbRefl2SpectCyan',   -- r += (g - r) * rgbRefl2SpectCyan;
                          V.map ((b-g)*) rgbRefl2SpectBlue')   -- r += (b - g) * rgbRefl2SpectBlue;
                    else (V.map ((b-r)*) rgbRefl2SpectCyan',   -- r += (b - r) * rgbRefl2SpectCyan;
                          V.map ((g-b)*) rgbRefl2SpectGreen')  -- r += (g - b) * rgbRefl2SpectGreen;
        in ret u v w
    | g<=r && g<=b =
        -- Compute reflectance _SampledSpectrum_ with _g_ as minimum
        let u     = V.map (g*) rgbRefl2SpectWhite'              -- r += g * rgbRefl2SpectWhite;
            (v,w) = if r<=b
                    then (V.map ((r-g)*) rgbRefl2SpectMagenta', -- r += (r - g) * rgbRefl2SpectMagenta;
                          V.map ((b-r)*) rgbRefl2SpectBlue')    -- r += (b - r) * rgbRefl2SpectBlue;
                    else (V.map ((b-g)*) rgbRefl2SpectMagenta', -- r += (b - g) * rgbRefl2SpectMagenta;
                          V.map ((r-b)*) rgbRefl2SpectRed')     -- r += (r - b) * rgbRefl2SpectRed;
        in ret u v w
    | otherwise = 
        -- Compute reflectance _SampledSpectrum_ with _b_ as minimum
        let u     = V.map (b*) rgbRefl2SpectWhite'             -- r += b * rgbRefl2SpectWhite;
            (v,w) = if r<=b
                    then (V.map ((r-b)*) rgbRefl2SpectYellow', -- r += (r - b) * rgbRefl2SpectYellow;
                          V.map ((g-r)*) rgbRefl2SpectGreen')  -- r += (g - r) * rgbRefl2SpectGreen;
                    else (V.map ((g-b)*) rgbRefl2SpectYellow', -- r += (g - b) * rgbRefl2SpectYellow;
                          V.map ((r-g)*) rgbRefl2SpectRed')    -- r += (r - g) * rgbRefl2SpectRed;
        in ret u v w
  where ret u v w = regularSPD' rgbToSpectrumCurves_start rgbToSpectrumCurves_end $
                                   V.zipWith3 (\x y z -> 0.94*(x+y+z)) u v w


regularSPD lambdaMin lambdaMax spectrum =
    regularSPD' lambdaMin lambdaMax $ V.fromList spectrum

regularSPD'   :: RealNum -> RealNum -> V.Vector RealNum -> SPD.SPD
regularSPD' lambdaMin' lambdaMax' spectrum' =
    let delta' = (lambdaMax' - lambdaMin') /
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

