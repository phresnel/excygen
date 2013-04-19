-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shapes.Shape
( FiniteShape(..)
) where

import Geometry(Ray)
import AABB
import Shapes.DifferentialGeometry(DifferentialGeometry)


-- Shape -----------------------------------------------------------------------

data FiniteShape a =  FiniteShape {
     aabb      :: AABB a,
     intersect :: Ray a -> Maybe (DifferentialGeometry a)
 }


