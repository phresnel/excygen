-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shapes.Sphere(
    sphere
)where

import Shapes.Shape
import Geometry
import AABB
import Shapes.DifferentialGeometry
import RGB
import Distance

-- Sphere ----------------------------------------------------------------------
data Sphere t = Sphere (Point t) t
                deriving(Show)

instance Shape Sphere where
    intersect = isectRaySphere

instance FiniteShape Sphere where
    aabb (Sphere (Point a b c) r) = AABB.aabb (Point (a-r) (b-r) (c-r))
                                              (Point (a+r) (b+r) (c+r))

sphere :: (Fractional t, Ord t) => Point t -> t -> Sphere t

-- Impl ------------------------------------------------------------------------
sphere point radius 
    | radius<0  = error "sphere radius must be positive"
    | otherwise = Sphere point radius 

isectRaySphere :: (Floating t, Ord t, RealFrac t) => 
                  Ray t -> Sphere t -> Maybe (DifferentialGeometry t)

isectRaySphere ray (Sphere center radius) =
  let
    (origin, direction) = ((ray_origin ray), (ray_direction ray))
    (Vector a b c) = origin `p_diff` center
    d0  = a*(d_u direction) + b*(d_v direction) + c*(d_w direction)
    d1  = d0^2
    d2  = (d_u direction)^2 + (d_v direction)^2 + (d_w direction)^2
    d3  = a^2 + b^2 + c^2
    discriminant = d1 - d2*(d3 - radius^2)    
  in if discriminant<0 then Nothing
     else let
       solA = -d0 - (sqrt discriminant)
       solB = -d0 + (sqrt discriminant)
     in if solA>0 then
            let dd = solA/d2 
            in Just DifferentialGeometry {d=distance dd,
                                          poi=ray_point ray dd,
                                          nn=normal 1 1 1,
                                          u=0, v=0 }
        else if solB>0 then
            let dd = solB/d2
            in Just DifferentialGeometry {d=distance dd,
                                          poi=ray_point ray dd,
                                          nn=normal 1 1 1,
                                          u=0, v=0 }
        else Nothing

