-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Primitives.Primitive
( Primitive(..),
  primitiveFromShape -- TODO: put in separate module
) where

import Geometry.Geometry(Ray)
import DifferentialGeometry(DifferentialGeometry)
import Intersection
import Shapes.Shape
import Photometry.SPD.SPD
import Photometry.SPD.Regular



---------------------------------------------------------------------------------------------------

data Primitive a = Primitive {
                      intersect :: Ray a -> Maybe (Intersection a)
                   }

primitiveFromShape :: RealFrac a => Shape a -> Primitive a
isectFromShape     :: RealFrac a => Shape a -> Ray a -> Maybe (Intersection a)



---------------------------------------------------------------------------------------------------

primitiveFromShape shape = 
        Primitive { 
            Primitives.Primitive.intersect = isectFromShape shape
        }


isectFromShape shape ray =
        let dg = Shapes.Shape.intersect shape ray
        in case dg of
            Just dg -> Just Intersection { differentialGeometry = dg,
                                           spd = regularSPD 100 600 [1] }
            Nothing -> Nothing

