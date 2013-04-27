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
import Intersection
import Primitives.Primitive



---------------------------------------------------------------------------------------------------

primitiveList :: RealFrac a => [Primitive a] -> Primitive a



---------------------------------------------------------------------------------------------------

primitiveList primitives = 
        Primitive { 
            Primitives.Primitive.intersect = isect primitives
        }



---------------------------------------------------------------------------------------------------

isect     :: RealFrac a => [Primitive a] -> Ray a -> Maybe (Intersection a)

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

