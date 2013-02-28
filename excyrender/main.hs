-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import Geometry
import PPM(toPPM)
import RGB
import Shape
import Sphere

raytrace :: (Fractional t, Shape s) => Int -> Int -> s t -> [RGB t]
raytrace width height shape =
    let trace_pixel x y = 
            let u' = fromIntegral(x) / fromIntegral(width)
                v' = 1 - fromIntegral(y) / fromIntegral(height)
            in RGB u' v' 0
    in [trace_pixel x y | y<-[0..height-1], x<-[0..width-1]]

ppm = 
  let width  = 64
      height = 48
      shape  = sphere (Point 0 0 5) 1
      pixels = raytrace width height shape
  in  toPPM width height pixels

main = putStrLn $ ppm
