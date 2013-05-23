-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module SurfaceIntegrators.VisualizeDistance (
  visualizeDistance
) where


import Primitives.Primitive
import Geometry.Point
import Geometry.Direction as D 
import Geometry.Ray as Ray
import DifferentialGeometry
import Photometry.RGB
import Intersection
import RealNum


visualizeDistance :: RealNum -> RealNum              -- configure: minT, maxT 
                  -> Primitive -> Ray -> Spectrum    -- integrator

visualizeDistance minT maxT primitive ray =
    let intersection = intersect primitive ray
    in case intersection of
       Just i -> let (Point x y z) = poi $ differentialGeometry i
                     f' = (sqrt (x^2+y^2+z^2) - minT) / (maxT - minT)
                     f = max 0 $ min 1 f'
                 in Spectrum.stretch $ spectrum 100 800 1 [1] $ f
       Nothing -> let dir = Ray.direction ray
                  in spectrum 100 800 1 [0]

