-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.Spectrum
( Spectrum, spectrum,
  spectrumFromSPD,
  spectrumFromRGB,
  gray, black,
  Photometry.Spectrum.toXYZ, toY,
  add, sub, mul, Photometry.Spectrum.stretch, pow, sum
) where

import Prelude hiding(min, max, sum)
import Photometry.SPD.SPD
import Photometry.SPD.Regular (regularSPDFromRGB)
import Photometry.CIEMatchingCurves
import Photometry.RGB (RGB(..))
import qualified Data.Vector.Unboxed as V
import RealNum


---------------------------------------------------------------------------------------------------
data Spectrum = Spectrum WavelengthMin WavelengthMax Intensities Delta InverseDelta
                deriving(Show)

type WavelengthMin = RealNum
type WavelengthMax = RealNum
type Delta         = RealNum
type InverseDelta  = RealNum
type Intensities   = V.Vector RealNum
type Resolution    = Int


spectrum        :: WavelengthMin -> WavelengthMax -> [RealNum] -> Spectrum
spectrumFromSPD :: WavelengthMin -> WavelengthMax -> Resolution -> SPD -> Spectrum
spectrumFromRGB :: WavelengthMin -> WavelengthMax -> Resolution -> RGB -> Spectrum
gray            :: WavelengthMin -> WavelengthMax -> Resolution -> RealNum -> Spectrum
black           :: WavelengthMin -> WavelengthMax -> Resolution -> Spectrum
toXYZ           :: Spectrum -> (RealNum,RealNum,RealNum)
toY             :: Spectrum -> RealNum -- sorry, the name is a bit bogus


add     :: Spectrum -> Spectrum -> Spectrum
sub     :: Spectrum -> Spectrum -> Spectrum
mul     :: Spectrum -> Spectrum -> Spectrum
stretch :: Spectrum -> RealNum -> Spectrum
pow     :: Spectrum -> RealNum -> Spectrum

sum     :: [Spectrum] -> Spectrum

---------------------------------------------------------------------------------------------------
gray min max res g =
    spectrumFromRGB min max res (RGB g g g)

black min max res = gray min max res 0

spectrumFromRGB min max res rgb = 
    spectrumFromSPD min max res $ regularSPDFromRGB rgb

spectrumFromSPD min max res spd = 
    let range = max - min
        f i = sample spd $ (i / fromIntegral res) * range + min
        delta = ((max - min) / (fromIntegral (res - 1)))
    in Spectrum min max (V.map f (V.enumFromN 0 res)) delta (1.0/delta)

spectrum min max bands = Spectrum min max bands' delta (1.0/delta)
                         where delta = (max - min) / (fromIntegral (V.length bands' - 1))
                               bands' = V.fromList bands

--toXYZ (Spectrum min max s) = Photometry.SPD.SPD.toXYZ $ regularSPD min max $ V.toList s

sampleRegular_unchecked :: RealNum -> V.Vector RealNum -> RealNum -> RealNum -> RealNum
sampleRegular_unchecked lambdaMin !spec inverseDelta lambda =
    let
      x = (lambda - lambdaMin) * inverseDelta
      b0 = floor x
      b1 = let a = (b0+1)
               b = ((V.length spec) - 1)
           in if a<b then a else b
      dx = x - fromIntegral b0
    in (1.0 - dx) * (spec V.! b0) + dx * (spec V.! b1)


sampleToCIELength :: Spectrum -> V.Vector RealNum
sampleToCIELength (Spectrum min max s _ inverseDelta) = 
   V.map f (V.enumFromN 0 cie_length)
   where f i = sampleRegular_unchecked min s inverseDelta 
                 (min + (i/fromIntegral cie_length) * (max-min))

toXYZ s =  
    let samples = sampleToCIELength s
    in (cie_inverse_length * V.sum (V.zipWith (*) cie_x' samples),
        cie_inverse_length * V.sum (V.zipWith (*) cie_y' samples),
        cie_inverse_length * V.sum (V.zipWith (*) cie_z' samples))

toY s =  
    let samples = sampleToCIELength s
    in cie_inverse_length * V.sum (V.zipWith (*) cie_y' samples)


add = binary (+)
sub = binary (-)
mul = binary (*)
stretch s x = map' (x*) s
pow s x     = map' (**x) s


sum []     = gray 300 830 54 0
sum [x]    = x
sum (x:xs) = x `add` sum xs


binary :: (RealNum -> RealNum -> RealNum) -> Spectrum -> Spectrum -> Spectrum
binary o (Spectrum min max l delta inverseDelta) (Spectrum min_r max_r r _ _)
  | min==min_r && max==max_r && V.length l==V.length r
        =  Spectrum min max (V.zipWith o l r) delta inverseDelta
  | otherwise = error ("Tried to operate on Spectrums of different topology ("
                 ++ "[" ++ show min ++ " " ++ show max ++ " " ++ show (V.length l) ++ "]"
                 ++ " vs. "
                 ++ "[" ++ show min_r ++ " " ++ show max_r ++ " " ++ show (V.length r) ++ "]"
                 ++ ")")

map' :: (RealNum -> RealNum) -> Spectrum -> Spectrum
map' o (Spectrum min max bands delta inverseDelta) = Spectrum min max (V.map o bands) delta inverseDelta

