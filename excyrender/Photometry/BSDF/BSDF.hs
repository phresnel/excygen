-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.BSDF.BSDF
( BSDF(..),
  diffuse,        -- TODO: just temporarily here
  specularReflect -- TOOD: just temporarily
) where

import Geometry.Direction as D
import Geometry.Vector as V
import Geometry.Normal as N
import Photometry.Spectrum
import RealNum
import Photometry.SPD.Regular


data BSDF = BSDF {
    pdf :: Direction -> Direction -> RealNum,
    f   :: Direction -> Direction -> Spectrum,
    sample_f :: Direction -> Normal -> (Direction, RealNum)
}


diffuse :: BSDF
diffuse = BSDF {
             pdf = \_ _ -> 1,
             f   = \_ _ -> spectrumFromSPD 100 600 1 $ regularSPD 100 600 [1],
             sample_f = \_ _ -> (direction 0 1 0, 0)
          }

specularReflect :: BSDF
specularReflect = BSDF {
             pdf = \_ _ -> 0,
             f   = \_ _ -> spectrum 100 600 [0],
             sample_f = \wo n -> (let cosI = -(n `N.dot'` wo)
                                      wo' = Vector (D.u wo) (D.v wo) (D.w wo)
                                      (Vector ix iy iz) = wo' `V.add` (N.stretch n (cosI*2))
                                  in direction ix iy iz
                                  , 1)
          }


