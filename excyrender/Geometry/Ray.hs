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


data Ray t = Ray (P.Point t) (Direction t)
             deriving (Show)

ray       :: P.Point t -> D.Direction t -> Ray t
direction :: Ray t -> D.Direction t
origin    :: Ray t -> P.Point t
point     :: (Num t, Ord t) => Ray t -> t -> P.Point t

ray origin direction = Ray origin direction
direction (Ray _ direction) = direction
origin    (Ray origin _)    = origin

point (Ray point direction) f
    | f<0       = error "ray_point undefined for negative f"
    | otherwise = P.add point (stretch direction f)
