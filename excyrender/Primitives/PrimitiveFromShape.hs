-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Primitives.PrimitiveFromShape
( primitiveFromShape
) where

import qualified Shapes.Shape as Sh
import qualified Photometry.BSDF.BSDF as BSDF
import qualified Geometry.Ray as R
import qualified Geometry.Point as P
import qualified Intersection as I
import qualified Primitives.Primitive as Pr


---------------------------------------------------------------------------------------------------

primitiveFromShape :: Sh.Shape -> Pr.Primitive
isectFromShape     :: Sh.Shape -> BSDF.BSDF -> R.Ray -> Maybe I.Intersection
occludesFromShape  :: Sh.Shape -> P.Point -> P.Point -> Bool

primitiveFromShape shape = 
        let bsdf = BSDF.specularReflect --BSDF {
                   --   f = \_ _ -> spectrumFromSPD 100 600 1 $ regularSPD 100 600 [1],
                   --   pdf = \_ _ -> 1
                   -- }
        in Pr.Primitive { 
            Pr.intersect = isectFromShape shape bsdf,
            Pr.occludes = occludesFromShape shape
        }


isectFromShape shape bsdf' ray =
        case Sh.intersect shape ray of
            Just dg -> Just I.Intersection { I.differentialGeometry = dg,
                                             I.bsdf = bsdf' }
            Nothing -> Nothing

occludesFromShape = Sh.occludes
