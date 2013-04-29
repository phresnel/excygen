-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shapes.Sphere(
    sphere
)where

import Shapes.Shape
import Geometry.Geometry
import Geometry.Point as P
import Geometry.Direction as D
import Geometry.Normal as N
import Geometry.Vector as V
import Geometry.Ray as Ray
import DifferentialGeometry



-- Sphere ----------------------------------------------------------------------
sphere :: (Floating a, Ord a, RealFrac a) => Point a -> a -> Shape a



-- Impl ------------------------------------------------------------------------
sphere center radius 
    | radius<0  = error "sphere radius must be positive"
    | otherwise = Shape {
                      intersect = isectRaySphere center radius,
                      occludes = occl center radius
                  }


isectRaySphere :: (Floating a, Ord a, RealFrac a) => 
                  Point a -> a -> Ray a -> Maybe (DifferentialGeometry a)

isectRaySphere center radius ray =
  let
    (origin, direction) = ((Ray.origin ray), (Ray.direction ray))
    (Vector a b c) = origin `P.diff` center
    d0  = a*(D.u direction) + b*(D.v direction) + c*(D.w direction)
    d1  = d0^2
    d2  = (D.u direction)^2 + (D.v direction)^2 + (D.w direction)^2
    d3  = a^2 + b^2 + c^2
    discriminant = d1 - d2*(d3 - radius^2)    
  in if discriminant<0 then Nothing
     else let
       solA = -d0 - (sqrt discriminant)
       solB = -d0 + (sqrt discriminant)
     in if solA>0 then
            let dd   = solA/d2
                poi' = Ray.point ray dd
            in Just DifferentialGeometry {d=dd,
                                          poi=poi',
                                          nn=let (Vector x y z) = poi' `diff` center
                                             in normal x y z,
                                          DifferentialGeometry.u=0,
                                          DifferentialGeometry.v=0 }
        else if solB>0 then
            let dd   = solB/d2
                poi' = Ray.point ray dd
            in Just DifferentialGeometry {d=dd,
                                          poi=poi',
                                          nn=let (Vector x y z) = poi' `diff` center
                                             in normal x y z,
                                          DifferentialGeometry.u=0,
                                          DifferentialGeometry.v=0 }
        else Nothing



occl :: (Floating a, Ord a, RealFrac a) 
     => Point a -> a -> Point a -> Point a -> Bool

occl center radius origin target = 
  let     
    direction = D.direction u v w
                where (Vector u v w) = target `P.diff` origin 
    (Vector a b c) = origin `P.diff` center
    d0  = a*(D.u direction) + b*(D.v direction) + c*(D.w direction)
    d1  = d0^2
    d2  = (D.u direction)^2 + (D.v direction)^2 + (D.w direction)^2
    d3  = a^2 + b^2 + c^2
    discriminant = d1 - d2*(d3 - radius^2)    
  in if discriminant<0 then False
     else let
       solA = -d0 - (sqrt discriminant)
       solB = -d0 + (sqrt discriminant)
     in solA>0 || solB>0

