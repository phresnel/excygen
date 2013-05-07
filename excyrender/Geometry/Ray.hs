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


import qualified Geometry.Point as P
import qualified Geometry.Direction as D
import RealNum


data Ray = Ray P.Point D.Direction
           deriving (Show)

ray       :: P.Point -> D.Direction -> Ray
direction :: Ray -> D.Direction
origin    :: Ray -> P.Point
point     :: Ray -> RealNum -> P.Point

ray o d = Ray o d
direction (Ray _ d) = d
origin    (Ray o _) = o

point (Ray p d) f
    | f<0       = error "ray_point undefined for negative f"
    | otherwise = P.add p (d `D.stretch` f)
