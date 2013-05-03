-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Integrators.Surface.PrimaryColor (
  primaryColor
) where


import Primitives.Primitive
import Geometry.Point
import Geometry.Direction as D 
import Geometry.Ray as Ray
import Photometry.RGB
import Intersection
import Photometry.ColorSpace
import Photometry.SPD.SPD
import Photometry.BSDF.BSDF
import RealNum


primaryColor :: Primitive -> Ray -> RGB

primaryColor primitive ray =
    let intersection = intersect primitive ray
    in case intersection of
       Just i -> let s = (f $ bsdf i) (D.direction 0 1 0) (D.direction 0 1 0)
                     (sr, sg, sb) = from_XYZ_to_sRGB . toXYZ $ s
                 in RGB sr sg sb
       Nothing -> let dir = Ray.direction ray
                  in RGB (D.u dir) (D.v dir) 0

