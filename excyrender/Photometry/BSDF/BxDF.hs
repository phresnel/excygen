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
import Geometry.Vector as V
import Geometry.Normal as N
import Photometry.Spectrum as Sp
import RealNum

data Distribution = Continuous | Specular deriving(Eq)
data ReflectionClass = Reflective | Transmissive

data BxDF = BxDF {
    pdf :: Direction -> Direction -> RealNum,
    f   :: Direction -> Direction -> Spectrum,
    sample_f :: Direction -> Normal -> (Direction, RealNum), -- TODO: instead of passing surface geometry, do everything locally
    distribution :: Distribution,
    reflection :: ReflectionClass    
}


lambertian :: Spectrum -> BxDF
lambertian s = BxDF {
             pdf = \_ _ -> 1,
             f   = \_ _ -> s `Sp.stretch` (1.0 / pi),
             sample_f = \_ _ -> (direction 0 1 0, 0),
             distribution = Continuous,
             reflection = Reflective
          }

specularReflect :: BxDF
specularReflect = BxDF {
             pdf = \_ _ -> 0,
             f   = \_ _ -> spectrum 100 600 [0],
             sample_f = \wo n -> (let cosI = -(n `N.dot'` wo)
                                      wo' = Vector (D.u wo) (D.v wo) (D.w wo)
                                      (Vector ix iy iz) = wo' `V.add` (N.stretch n (cosI*2))
                                  in direction ix iy iz
                                  , 1),
             distribution = Specular,
             reflection = Reflective
          }


