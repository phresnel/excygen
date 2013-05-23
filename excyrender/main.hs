-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import qualified Geometry.Ray as Ray
import qualified Geometry.Direction as D
import qualified Geometry.Point as P
import qualified Geometry.Normal as N

import ImageFormat.PPM(toPPM)

import Photometry.RGB
import qualified Photometry.BSDF.BSDF as BSDF
import qualified Photometry.BSDF.BxDF as X
import Photometry.ColorSpace
import Photometry.Spectrum as Spectrum

import Shapes.Sphere
import qualified Shapes.Plane as Plane

import Primitives.Primitive
import Primitives.PrimitiveFromShape
import Primitives.PrimitiveList

import Integrators.Surface.Whitted

import Control.Parallel.Strategies


-- simple renderer -------------------------------------------------------------
raytrace :: Int -> Int -> Primitive -> (Primitive -> Ray.Ray -> Spectrum) -> [RGB]
raytrace width height primitive surface_integrator =
    map trace_pixel [0..(width*height)-1]
    where trace_pixel p =             
            let u = fromIntegral (p `mod` width) / fromIntegral width
                v = 1 - fromIntegral (p `div` width) / fromIntegral height
                ray = Ray.Ray (P.Point 0 0 0) (D.direction (u-0.5) (v-0.5) 1)
                (sR, sG, sB) = from_XYZ_to_sRGB . Spectrum.toXYZ $ surface_integrator primitive ray
            in (RGB sR sG sB)



ppm :: String
ppm = 
  let width  = 320 
      height = 320 
      primitive'  = primitiveList [
                     primitiveFromShape (sphere (P.Point (-1.0) 0.0 5) 1)
                                        (BSDF.bsdf [X.lambertian (spectrum 100 600 [1])]),
                     primitiveFromShape (sphere (P.Point 1.0 0.5 5) 1)
                                        (BSDF.bsdf [X.lambertian (spectrum 100 600 [0.75]),
                                                    X.specularReflect (spectrum 100 600 [0.25])
                                                   ]),
                     primitiveFromShape (Plane.fromPointNormal (P.Point (0) (-1) 0) (N.normal 0 1 0))
                                        (BSDF.bsdf [X.lambertian (spectrum 100 600 [0.5]),
                                                    X.specularReflect (spectrum 100 600 [0.25])
                                                   ])
                    ]
      !primitive = primitive'
      pixels = raytrace width height primitive whitted `using` parListChunk (512) rdeepseq
  
  in  toPPM width height pixels

main :: IO ()
main = putStrLn $ ppm
