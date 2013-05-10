-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.BSDF.BSDF
( BSDF, bsdf,
  f, sample_f -- TODO: sample_f should probably take a DifferentialGeometry
) where

import Photometry.Spectrum as Sp
import RealNum
import qualified Photometry.BSDF.BxDF as X 

import Geometry.Direction as D
import Geometry.Normal as N


---------------------------------------------------------------------------------------------------


type Probability = RealNum
data BSDF = BSDF [(X.BxDF, Probability)]

bsdf     :: [(X.BxDF,Probability)] -> BSDF
--pdf      :: BSDF -> Direction -> Direction -> RealNum
f        :: BSDF -> Direction -> Direction -> Spectrum
sample_f :: BSDF -> Direction -> Normal -> (Direction, RealNum) -- TODO: those should all return Maybe

bsdf xs
  | length xs == 0           = error "BSDF must have one or more BxDFs"
  | totalPdf<0 || totalPdf>1 = error "BSDF probability must sum up to [0..1]" 
  | otherwise                = BSDF xs
  where totalPdf = foldr (\(_, p) a -> a+p) 0 xs

f (BSDF xs) wo wi    = f' xs wo wi

f' :: [(X.BxDF, Probability)] -> Direction -> Direction -> Spectrum
f' [] _ _ = error "BSDF.hs: <f' [] wo wi> should not be reachable at all for the outside world"
f' [(bxdf, p)] wo wi
  | isContinuous = X.f bxdf wo wi `Sp.stretch` p
  | otherwise    = Sp.spectrum 100 600 [0]
  where isContinuous = (X.distribution bxdf) == X.Continuous
f' (x:xs) wo wi = f' [x] wo wi `Sp.add` f' xs wo wi


sample_f (BSDF pbxdfs) wo n =
   let bxdfs = filter (\(bxdf, _) -> X.distribution bxdf == X.Specular) pbxdfs
   in if null bxdfs then (direction 0 1 0, 0)
                    else let (bxdf, p') = head bxdfs
                             (wi, p) = X.sample_f bxdf wo n
                         in (wi, p' * p)

