-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import Geometry(Ray(..),ray_direction,Point(..),direction,d_u,d_v)

import ImageFormat.PPM(toPPM)

import Photometry.RGB
import Photometry.ColorSpace

import Shapes.Shape
import Shapes.Sphere

import Intersection
import DifferentialGeometry
import Primitives.Primitive


--data Radiance t = Radiance t t t
--radianceFromRGB :: RGB t -> Radiance t
--radianceFromRGB (RGB r g b) = Radiance r g b 
--radianceToRGB :: Radiance t -> RGB t
--radianceToRGB (Radiance r g b) = RGB r g b


-- integrator for only primary intersections -----------------------------------
primary :: (RealFrac t, Floating t) => Primitive t -> Ray t -> RGB t

primary primitive ray =
    let intersection = Primitives.Primitive.intersect primitive ray
    in case intersection of
       Just i -> let (Point x y z) = poi $ differentialGeometry i
                     f = sqrt (x^2+y^2+z^2) / 6.0 
                 in RGB f f f
       Nothing -> let dir = ray_direction ray
                  in RGB (d_u dir) (d_v dir) 0



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
            let ray = Ray (Point 0 0 0) (direction (u-0.5) (v-0.5) 1)
            in surface_integrator primitive ray



ppm = 
  let width  = 64
      height = 64
      primitive  = primitiveFromShape $ sphere (Point 0 0 5) 1
      pixels = raytrace width height primitive primary
  in  toPPM width height pixels

main = putStrLn $ ppm
