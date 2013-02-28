-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import Geometry
import PPM(toPPM)
import RGB
import Shape
import Sphere

data ImageSize = ImageSize { width :: Int, height :: Int }

data Radiance t = Radiance t t t
radianceFromRGB :: RGB t -> Radiance t
radianceFromRGB (RGB r g b) = Radiance r g b 
radianceToRGB :: Radiance t -> RGB t
radianceToRGB (Radiance r g b) = RGB r g b

-- rendering function
recursive :: (Fractional t, Shape s) => s t -> Ray t -> Radiance t
recursive shape ray = 
    let d = ray_direction ray
    in radianceFromRGB $ RGB (d_u d) (d_v d) (d_w d)  

-- function that creates a primary ray and calls the rendering function
raytracePixel :: (Floating t, Shape s) => s t -> t -> t -> RGB t
raytracePixel shape u v = 
    let ray = Geometry.ray (Point 0 0 0) (direction u v 1)
        radiance = recursive shape ray
    in radianceToRGB radiance

-- constructs an image by raytracing over all pixels
raytraceImage :: (Floating t, Shape s) => ImageSize -> s t -> [RGB t]
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
