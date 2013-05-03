-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Primitives.Primitive
( Primitive(..),
) where

import Geometry.Ray
import Geometry.Point
import Intersection



---------------------------------------------------------------------------------------------------

data Primitive = Primitive {
                    intersect :: Ray -> Maybe Intersection,
                    occludes  :: Point -> Point -> Bool
                 }

