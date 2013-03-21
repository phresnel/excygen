-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import Geometry(Ray(..),Point(..),direction)
import PPM(toPPM)
import RGB
import Shape
import Sphere

raytrace :: (RealFrac t, Floating t, Shape s) => Int -> Int -> s t -> [RGB t]
raytrace width height shape =
    -- TODO just do a list comprehension?
    let raytrace_rows y | y==height = []
                        | otherwise = scanline 0 y ++ raytrace_rows (y+1)
        scanline x y    | x==width  = []
                        | otherwise = (trace_pixel x y) : scanline (x+1) y
        trace_pixel x y = 
            let u = fromIntegral(x) / fromIntegral(width)
                v = 1 - fromIntegral(y) / fromIntegral(height)
                ray = Ray (Point 0 0 0) (direction (u-0.5) (v-0.5) 1)
                does_intersect = intersect ray shape
            in if does_intersect
               then RGB u v 1
               else RGB u v 0
    in raytrace_rows 0


ppm = 
  let width  = 640
      height = 640
      shape  = sphere (Point 0 0 5) 1
      pixels = raytrace width height shape
  in  toPPM width height pixels

main = putStrLn $ ppm
