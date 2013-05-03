-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Ray (
  Ray(..),
  ray,
  Geometry.Ray.origin,
  Geometry.Ray.direction,
  Geometry.Ray.point
) where


import Geometry.Point as P
import Geometry.Direction as D
import RealNum


data Ray = Ray P.Point Direction
           deriving (Show)

ray       :: P.Point -> D.Direction -> Ray
direction :: Ray -> D.Direction
origin    :: Ray -> P.Point
point     :: Ray -> RealNum -> P.Point

ray origin direction = Ray origin direction
direction (Ray _ direction) = direction
origin    (Ray origin _)    = origin

point (Ray point direction) f
    | f<0       = error "ray_point undefined for negative f"
    | otherwise = P.add point (stretch direction f)
