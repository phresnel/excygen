-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Sphere(
    Sphere
)where

import Shape
import Geometry
import AABB

-- Sphere ----------------------------------------------------------------------
data Sphere t = Sphere (Point t) t
                deriving(Show)

instance Shape Sphere where
    intersect = isectRaySphere

instance FiniteShape Sphere where
    aabb (Sphere (Point a b c) r) = AABB.aabb (Point (a-r) (b-r) (c-r))
                                              (Point (a+r) (b+r) (c+r))


-- Impl ------------------------------------------------------------------------
isectRaySphere :: (Floating t, Ord t, RealFrac t) => 
                  Ray t -> Sphere t -> Bool -- Maybe (Intersection t)
isectRaySphere (Ray origin direction)
               (Sphere center radius) =
  let
    (Vector a b c) = origin `p_diff` center
    d0  = a*(d_u direction) + b*(d_v direction) + c*(d_w direction)
    d1  = d0^2
    d2  = (d_u direction)^2 + (d_v direction)^2 + (d_w direction)^2
    d3  = a^2 + b^2 + c^2
    discriminant = d1 - d2*(d3 - radius^2)    
  in if discriminant<0 then False -- Nothing
     else let
       solA = -d0 - (sqrt discriminant)
       solB = -d0 + (sqrt discriminant)      
     in if solA>0 then True -- Just $ Intersection (solA/d2)
        else if solB>0 then True -- Just $ Intersection (solB/d2)
        else False -- Nothing

