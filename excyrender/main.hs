-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import qualified Geometry.Ray as Ray
import qualified Geometry.Direction as D
import qualified Geometry.Point as P
import qualified Geometry.Normal as N

import ImageFormat.PPM(toPPM)

import Photometry.RGB as RGB
import qualified Photometry.BSDF.BSDF as BSDF
import qualified Photometry.BSDF.BxDF as X
import Photometry.ColorSpace
import Photometry.Spectrum as Spectrum

import Shapes.Sphere
import qualified Shapes.Plane as Plane

import Primitives.Primitive
import Primitives.PrimitiveFromShape
import Primitives.PrimitiveList

import SurfaceIntegrators.Path

import Control.Parallel.Strategies
import System.Random
import RealNum


-- simple renderer -------------------------------------------------------------
raytrace :: Int -> Int -> Primitive -> (Primitive -> Ray.Ray -> [RealNum] -> (Spectrum,[RealNum])) -> [RGB]
raytrace width height primitive surface_integrator =
    map (trace_pixel (4::Int)) [0..(width*height)-1]
    where trace_pixel samples p =
           RGB.sum $ map 
                      (\i -> 
                       let x = p `mod` width
                           y = p `div` width
                           u = (fromIntegral x) / (fromIntegral width)
                           v = 1 - (fromIntegral y) / (fromIntegral height)
                           ray = Ray.Ray (P.Point 0 0 0) (D.direction (u-0.5) (v-0.5) 1)
                           (incoming, _) = surface_integrator primitive ray (randoms $ mkStdGen (i*104327+y*4909+x*60331))
                           (sR, sG, sB) = from_XYZ_to_sRGB . Spectrum.toXYZ $ incoming
                       in (RGB sR sG sB) `shrink` fromIntegral samples)
                      [0..samples]



ppm :: String
ppm = 
  let width  = 256
      height = 256
      primitive'  = primitiveList [
                     primitiveFromShape (sphere (P.Point (-1.0) 0.0 5) 1)
                                        (BSDF.bsdf [X.lambertian (spectrum 100 600 [0.8])]),
                     primitiveFromShape (sphere (P.Point 1.0 0.5 5) 1)
                                        (BSDF.bsdf [--X.lambertian (spectrum 100 600 [1.0])
                                                   X.specularReflect (spectrum 100 600 [0.8])
                                                   ]),
                     primitiveFromShape (Plane.fromPointNormal (P.Point (0) (-1) 0) (N.normal 0 1 0)) (BSDF.bsdf [X.lambertian (spectrum 100 600 [0.8])]),
                     primitiveFromShape (Plane.fromPointNormal (P.Point (-1) (-1) 0) (N.normal 1 0 0)) (BSDF.bsdf [X.lambertian (spectrum 100 600 [0.8])])
                    ]
      !primitive = primitive'
      pixels = raytrace width height primitive path `using` parListChunk (512) rdeepseq
  
  in  toPPM width height pixels

main :: IO ()
main = putStrLn $ ppm
