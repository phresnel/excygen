-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shapes.Shape
( Shape(..)
) where

import Geometry.Ray
import Geometry.Point
import DifferentialGeometry(DifferentialGeometry)


-- Shape -----------------------------------------------------------------------
data Shape a = Shape {
     intersect :: Ray a -> Maybe (DifferentialGeometry a),
     occludes :: Point a -> Point a -> Bool
 }


