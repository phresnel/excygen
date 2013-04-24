-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Integrators.Surface.VisualizeDistance (
  visualizeDistance
) where


import Primitives.Primitive
import Geometry.Point
import Geometry.Direction as D 
import Geometry.Ray as Ray
import DifferentialGeometry
import Photometry.RGB
import Intersection


visualizeDistance :: (RealFrac t, Floating t) 
                  => t -> t                           -- configure: minT, maxT 
                  -> Primitive t -> Ray t -> RGB t    -- integrator

visualizeDistance minT maxT primitive ray =
    let intersection = intersect primitive ray
    in case intersection of
       Just i -> let (Point x y z) = poi $ differentialGeometry i
                     f' = (sqrt (x^2+y^2+z^2) - minT) / (maxT - minT)
                     f = max 0 $ min 1 f'
                 in RGB f f f
       Nothing -> let dir = Ray.direction ray
                  in RGB (D.u dir) (D.v dir) 0

