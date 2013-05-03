-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Primitives.PrimitiveList
( primitiveList
) where

import DifferentialGeometry(DifferentialGeometry)
import Shapes.Shape
import Photometry.SPD.SPD
import Photometry.SPD.Regular
import Photometry.BSDF.BSDF
import Geometry.Ray
import Geometry.Point
import Intersection
import Primitives.Primitive



---------------------------------------------------------------------------------------------------

primitiveList :: [Primitive] -> Primitive



---------------------------------------------------------------------------------------------------

primitiveList primitives = 
        Primitive { 
            Primitives.Primitive.intersect = isect primitives,
            Primitives.Primitive.occludes = occl primitives
        }



---------------------------------------------------------------------------------------------------

isect     :: [Primitive] -> Ray -> Maybe Intersection
occl      :: [Primitive] -> Point -> Point -> Bool

isect [] _ = Nothing

isect (x:xs) ray =
    let other' = isect xs ray
    in case Primitives.Primitive.intersect x ray of
     Just current -> case other' of
                       Just other -> if distance current < distance other
                                     then Just current
                                     else other'
                       Nothing -> Just current
     Nothing -> other'


occl [] _ _ = False
occl (x:xs) p q 
   | Primitives.Primitive.occludes x p q = True
   | otherwise = occl xs p q

