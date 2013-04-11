-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module SPD.Regular
( regularSPD,
  sample,
  toXYZ
) where

import SPD
import CIEMatchingCurves



-- Regular -------------------------------------------------------------------------------------
data Regular a = Regular {
                     lambdaMin :: a,
                     lambdaMax :: a,
                     spectrum :: [a],
                     delta :: a,
                     inverseDelta :: a
                 }
                 deriving (Show)


instance SPD Regular where
    sample = sampleRegular
    toXYZ = toXYZRegular
    

regularSPD    :: (RealFrac a) => a -> a -> [a] -> Regular a
sampleRegular :: (RealFrac t) => (Regular t) -> t -> t 
toXYZRegular  :: (RealFrac t) => (Regular t) -> (t, t, t)



-- impl -------------------------------------------------------------------------------------------
regularSPD lambdaMin' lambdaMax' spectrum' =
    let delta' = (lambdaMax' - lambdaMin') /
                 (fromIntegral ((length spectrum') - 1))
    in Regular {
           lambdaMin = lambdaMin',
           lambdaMax = lambdaMax',
           spectrum = spectrum',
           delta = delta',
           inverseDelta = 1.0 / delta'
       }


sampleRegular (Regular lambdaMin lambdaMax spectrum _ inverseDelta) lambda
    | lambda < lambdaMin = 0
    | lambda > lambdaMax = 0
    | otherwise = let
        x = (lambda - lambdaMin) * inverseDelta
        b0 = floor x
        b1 = min (b0+1) ((length spectrum) - 1)
        dx = x - fromIntegral b0
      in (1.0 - dx) * spectrum!!b0 + dx * spectrum!!b1


toXYZRegular spd@(Regular lambdaMin _ _ _ inverseDelta) =  
    let 
        samp i = sample spd $ lambdaMin + inverseDelta * fromIntegral i
        integrate curve = (sum $ map (\i -> curve!!i * samp i) [0..cie_length]) * cie_inverse_length
    in
        (integrate cie_x, integrate cie_y, integrate cie_z)


