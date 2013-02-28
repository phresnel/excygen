-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import Geometry
import PPM(toPPM)
import RGB
import Shape
import Sphere

data ImageSize = ImageSize { width :: Int, height :: Int }

raytracePixel :: (Fractional t, Shape s) => s t -> t -> t -> RGB t
raytracePixel shape u v = RGB u v 0

raytraceImage :: (Fractional t, Shape s) => ImageSize -> s t -> [RGB t]
raytraceImage size shape = [let u = fromIntegral(x) / fromIntegral(width size)
                                v = fromIntegral(y) / fromIntegral(height size)
                            in raytracePixel shape u v
                           | y<-[0..(height size)-1]
                           , x<-[0..(width size)-1]]

ppm = 
  let width  = 64
      height = 48
      shape  = sphere (Point 0 0 5) 1
      pixels = raytraceImage ImageSize{width=width,height=height} shape
  in  toPPM width height pixels

main = putStrLn $ ppm
