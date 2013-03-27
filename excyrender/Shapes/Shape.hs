-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shapes.Shape
( Shape(..)
, FiniteShape(..)
) where

import Geometry(Point, Ray, direction)
import AABB
import Shapes.DifferentialGeometry(DifferentialGeometry)


-- Shape -----------------------------------------------------------------------
class Shape s where
    intersect :: (RealFrac t, Floating t, Ord t, Shape s) =>
     Ray t -> s t -> Maybe (DifferentialGeometry t)

class (Shape s) => FiniteShape s where
    aabb :: (Num t, Ord t) => s t -> AABB t


