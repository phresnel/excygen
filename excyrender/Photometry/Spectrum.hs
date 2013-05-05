-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.Spectrum
( Spectrum, spectrum,
  spdToSpectrum,
  toXYZ,
  add, sub, stretch, pow
) where

import Photometry.SPD.SPD
import Photometry.SPD.Regular

import Data.Vector.Unboxed as V
import RealNum


---------------------------------------------------------------------------------------------------
data Spectrum = Spectrum WavelengthMin WavelengthMax Intensities
                deriving(Show)

type WavelengthMin = RealNum
type WavelengthMax = RealNum
type Intensities   = V.Vector RealNum
type Resolution    = Int


spdToSpectrum :: WavelengthMin -> WavelengthMax -> Resolution -> SPD -> Spectrum
toXYZ         :: Spectrum -> (RealNum,RealNum,RealNum)


add     :: Spectrum -> Spectrum -> Spectrum
sub     :: Spectrum -> Spectrum -> Spectrum
stretch :: Spectrum -> RealNum -> Spectrum
pow     :: Spectrum -> RealNum -> Spectrum



---------------------------------------------------------------------------------------------------
spdToSpectrum min max res spd = 
    let range = max - min
        f i = sample spd $ (i / fromIntegral res) * range + min
    in Spectrum min max $ V.map f (V.enumFromN 0 res)

spectrum min max bands = Spectrum min max $ V.fromList bands 

toXYZ (Spectrum min max s) = Photometry.SPD.SPD.toXYZ $ regularSPD min max $ V.toList s

add = binary (+)
sub = binary (-)
stretch s x = map' (x*) s
pow s x     = map' (^x) s


binary o (Spectrum min max l) (Spectrum min_r max_r r)
  | min==min_r && max==max_r && V.length l==V.length r
        =  Spectrum min max $ V.zipWith o l r
  | otherwise = error "Tried to operate on Spectrums of different topology"

map' o (Spectrum _ _ bands) = V.map o bands

