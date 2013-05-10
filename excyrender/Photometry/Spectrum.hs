-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.Spectrum
( Spectrum, spectrum,
  spectrumFromSPD,
  Photometry.Spectrum.toXYZ,
  add, sub, mul, Photometry.Spectrum.stretch, pow, sum
) where

import Prelude hiding(min, max, sum)
import Photometry.SPD.SPD
import Photometry.SPD.Regular

import qualified Data.Vector.Unboxed as V
import RealNum


---------------------------------------------------------------------------------------------------
data Spectrum = Spectrum WavelengthMin WavelengthMax Intensities
                deriving(Show)

type WavelengthMin = RealNum
type WavelengthMax = RealNum
type Intensities   = V.Vector RealNum
type Resolution    = Int


spectrum        :: WavelengthMin -> WavelengthMax -> [RealNum] -> Spectrum
spectrumFromSPD :: WavelengthMin -> WavelengthMax -> Resolution -> SPD -> Spectrum
toXYZ           :: Spectrum -> (RealNum,RealNum,RealNum)


add     :: Spectrum -> Spectrum -> Spectrum
sub     :: Spectrum -> Spectrum -> Spectrum
mul     :: Spectrum -> Spectrum -> Spectrum
stretch :: Spectrum -> RealNum -> Spectrum
pow     :: Spectrum -> RealNum -> Spectrum

sum     :: [Spectrum] -> Spectrum

---------------------------------------------------------------------------------------------------
spectrumFromSPD min max res spd = 
    let range = max - min
        f i = sample spd $ (i / fromIntegral res) * range + min
    in Spectrum min max $ V.map f (V.enumFromN 0 res)

spectrum min max bands = Spectrum min max $ V.fromList bands 

toXYZ (Spectrum min max s) = Photometry.SPD.SPD.toXYZ $ regularSPD min max $ V.toList s

add = binary (+)
sub = binary (-)
mul = binary (*)
stretch s x = map' (x*) s
pow s x     = map' (**x) s


sum []     = spectrum 100 600 [0]
sum [x]    = x
sum (x:xs) = x `add` sum xs


binary :: (RealNum -> RealNum -> RealNum) -> Spectrum -> Spectrum -> Spectrum
binary o (Spectrum min max l) (Spectrum min_r max_r r)
  | min==min_r && max==max_r && V.length l==V.length r
        =  Spectrum min max $ V.zipWith o l r
  | otherwise = error ("Tried to operate on Spectrums of different topology ("
                 ++ "[" ++ show min ++ " " ++ show max ++ " " ++ show (V.length l) ++ "]"
                 ++ " vs. "
                 ++ "[" ++ show min_r ++ " " ++ show max_r ++ " " ++ show (V.length r) ++ "]"
                 ++ ")")

map' :: (RealNum -> RealNum) -> Spectrum -> Spectrum
map' o (Spectrum min max bands) = Spectrum min max $ V.map o bands

