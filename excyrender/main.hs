-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import Geometry(Ray(..),ray_direction,Point(..),direction,d_u,d_v)
import PPM(toPPM)
import RGB
import Shape
import Sphere
import Intersection

-- integrator for only primary intersections -----------------------------------
primary :: (RealFrac t, Floating t, Shape s) => Ray t -> s t -> RGB t
primary ray shape =
    let intersection = intersect ray shape
    in case intersection of
       Just i -> diffuse i
       Nothing -> let dir = ray_direction ray
                  in RGB (d_u dir) (d_v dir) 0


-- simple renderer -------------------------------------------------------------
raytrace :: (RealFrac t, Floating t, Shape s)
    => Int -> Int -> s t -> (Ray t -> s t -> RGB t) -> [RGB t]
raytrace width height shape surface_integrator =
    -- TODO just do a list comprehension?
    let raytrace_rows y | y==height = []
                        | otherwise = scanline 0 y ++ raytrace_rows (y+1)
        scanline x y    | x==width  = []
                        | otherwise = (trace_pixel x y) : scanline (x+1) y
        trace_pixel x y = 
            let u = fromIntegral(x) / fromIntegral(width)
                v = 1 - fromIntegral(y) / fromIntegral(height)
                ray = Ray (Point 0 0 0) (direction (u-0.5) (v-0.5) 1)
            in surface_integrator ray shape
    in raytrace_rows 0


ppm = 
  let width  = 64
      height = 64
      shape  = sphere (Point 0 0 5) 1
      pixels = raytrace width height shape primary
  in  toPPM width height pixels

main = putStrLn $ ppm
