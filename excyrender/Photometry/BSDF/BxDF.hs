-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.BSDF.BxDF
( BxDF(..),
  Distribution(..),
  ReflectionClass(..),
  lambertian,     -- TODO: just temporarily here
  specularReflect -- TOOD: just temporarily
) where

import Geometry.Direction as D
import Photometry.Spectrum as Sp
import RealNum

data Distribution = Continuous | Specular deriving(Eq)
data ReflectionClass = Reflective | Transmissive deriving(Eq)

data BxDF = BxDF {
    pdf :: Direction -> Direction -> RealNum,
    f   :: Direction -> Direction -> Spectrum,
    sample_f :: Direction -> [RealNum] -> (Direction, Spectrum, RealNum, [RealNum]),
    distribution :: Distribution,
    reflection :: ReflectionClass    
}


--randomDir :: [RealNum] -> (Direction, [RealNum])
--randomDir (x:y:z:rest) =
--  let (x',y',z') = (x*2-1,y*2-1,z*2-1)
--  in if x'*x'+y'*y'+z'*z'<1
--  then (direction x' y' z', rest)
--  else randomDir rest
--randomDir _ = error "not enough pseudo random numbers left"


lambertian :: Spectrum -> BxDF
lambertian s = let s' = s `Sp.stretch` (1.0 / pi)
                   pdf' = \_ wi -> abs (D.v wi) / pi
               in BxDF {
                    pdf = pdf',
                    f   = \_ _ -> s',
                    sample_f = \wo randoms -> 
                                let (wi, randoms') = D.cosineWeightedHemisphere randoms
                                in (wi,
                                    s',
                                    pdf' wo wi,
                                    randoms'),
                    distribution = Continuous,
                    reflection = Reflective
                 }

specularReflect :: Spectrum -> BxDF
specularReflect s = BxDF {
             pdf = \_ _ -> 0,
             f   = \_ _ -> spectrum 100 600 [0],
             sample_f = \wo randoms -> 
                          let rdir = D.direction (-(D.u wo)) (D.v wo) (-(D.w wo)) 
                          in (rdir,
                              s `Sp.stretch` (1 / abs (D.v rdir)),
                              1,
                              randoms),
             distribution = Specular,
             reflection = Reflective
          }


