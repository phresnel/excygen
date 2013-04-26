-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import Geometry.Ray as Ray
import Geometry.Direction as D
import Geometry.Point

import ImageFormat.PPM(toPPM)

import Photometry.RGB
import Photometry.ColorSpace

import Shapes.Shape
import Shapes.Sphere

import Intersection
import DifferentialGeometry
import Primitives.Primitive

import Integrators.Surface.Whitted

--data Radiance t = Radiance t t t
--radianceFromRGB :: RGB t -> Radiance t
--radianceFromRGB (RGB r g b) = Radiance r g b 
--radianceToRGB :: Radiance t -> RGB t
--radianceToRGB (Radiance r g b) = RGB r g b


-- simple renderer -------------------------------------------------------------
raytrace :: (RealFrac t, Floating t)
    => Int -> Int -> Primitive t -> (Primitive t -> Ray t -> RGB t) -> [RGB t]
raytrace width height primitive surface_integrator =
    [let u = fromIntegral(x) / fromIntegral(width)
         v = 1 - fromIntegral(y) / fromIntegral(height)
     in trace_pixel u v
    | y<-[0..height-1]
    , x<-[0..width-1]]
    where trace_pixel u v = 
            let ray = Ray.Ray (Point 0 0 0) (D.direction (u-0.5) (v-0.5) 1)
            in surface_integrator primitive ray



ppm = 
  let width  = 64
      height = 64
      primitive  = primitiveFromShape $ sphere (Point 0 0 5) 1
      pixels = raytrace width height primitive
                        whitted
  in  toPPM width height pixels

main = putStrLn $ ppm
