-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Primitive
( Primitive(..),
  PrimitiveFromShape(..) -- TODO: put in separate module
) where

import Geometry(Point, Ray, direction)
import Shapes.DifferentialGeometry(DifferentialGeometry)
import Intersection
import Shapes.Shape

-- Primitive --------------------------------------------------------------------------------------

class Primitive p where
    intersect :: (RealFrac t, Floating t, Ord t, Primitive p)
              => Ray t -> p t -> Maybe (Intersection t)


data PrimitiveFromShape s t = PrimitiveFromShape (s t)

instance (Shape s) => Primitive (PrimitiveFromShape s) where
    intersect ray (PrimitiveFromShape shape) = 
        case Shapes.Shape.intersect ray shape of
            Just dg -> Just Intersection { differentialGeometry = dg }
            Nothing -> Nothing

