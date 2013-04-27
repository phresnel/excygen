-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Primitives.PrimitiveFromShape
( primitiveFromShape
) where

import DifferentialGeometry(DifferentialGeometry)
import Shapes.Shape
import Photometry.SPD.SPD
import Photometry.SPD.Regular
import Photometry.BSDF.BSDF
import Geometry.Ray
import Intersection
import Primitives.Primitive


---------------------------------------------------------------------------------------------------

primitiveFromShape :: RealFrac a => Shape a -> Primitive a
isectFromShape     :: RealFrac a => Shape a -> Ray a -> Maybe (Intersection a)

primitiveFromShape shape = 
        Primitive { 
            Primitives.Primitive.intersect = isectFromShape shape
        }


isectFromShape shape ray =
        let dg = Shapes.Shape.intersect shape ray
        in case dg of
            Just dg -> Just Intersection { differentialGeometry = dg,
                                           bsdf = BSDF { --regularSPD 100 600 [1]
                                                           f = \_ _ -> regularSPD 100 600 [1]
                                                       }
                                         }
            Nothing -> Nothing

