-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.BSDF.BSDF
( BSDF, bsdf,
  f, pdf, sample_f -- TODO: sample_f should probably take a DifferentialGeometry
) where

import Photometry.Spectrum as Sp
import RealNum
import qualified Photometry.BSDF.BxDF as X 

import Geometry.Direction as D
import Geometry.Normal as N


---------------------------------------------------------------------------------------------------


data BSDF = BSDF [X.BxDF]

bsdf     :: [X.BxDF] -> BSDF
pdf      :: BSDF -> Direction -> Direction -> RealNum
f        :: BSDF -> Direction -> Direction -> Spectrum
sample_f :: BSDF -> Direction -> Normal -> (Direction, Spectrum, RealNum) -- TODO: those should all return Maybe

bsdf [] = error "BSDF must have one or more BxDFs"
bsdf xs = BSDF xs

f (BSDF xs) wo wi =
    let bxdfs = filter (\bxdf -> X.distribution bxdf == X.Continuous) xs
    in if null bxdfs then (Sp.spectrum 100 600 [0])
       else foldr1 Sp.add . map (\bxdf -> X.f bxdf wo wi) $ bxdfs

pdf (BSDF xs) wo wi =
    let bxdfs = filter (\bxdf -> X.distribution bxdf == X.Continuous) xs
    in if null bxdfs then 0
       else sum $ map (\bxdf -> (X.pdf bxdf wo wi)) $ bxdfs

sample_f (BSDF xs) wo n =
   let bxdfs = filter (\bxdf -> X.distribution bxdf == X.Specular) xs
   in if null bxdfs then (direction 0 1 0, Sp.spectrum 100 600 [0], 0)
      else if length bxdfs /= 1 then error "BSDF currently supports up to 1 specular BxDFs"
      else let bxdf = head bxdfs
           in X.sample_f bxdf wo n

