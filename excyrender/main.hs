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

import Photometry.Background.Preetham.Preetham

import Shapes.Sphere
import qualified Shapes.Plane as Plane

import Primitives.PrimitiveFromShape
import Primitives.PrimitiveList

import SurfaceIntegrators.Path
import Photometry.Lighting as Lighting

import Control.Parallel.Strategies
import System.Random
import RealNum


-- simple renderer -------------------------------------------------------------
raytrace :: Int -> Int -> (Ray.Ray -> [RealNum] -> (Spectrum,[RealNum])) -> [RGB]
raytrace width height surface_integrator =
    map (trace_pixel (100::Int)) [0..(width*height)-1]
    where trace_pixel samples p =
           RGB.sum $ map 
                      (\i -> 
                       let x = p `mod` width
                           y = p `div` width
                           u = (fromIntegral x) / (fromIntegral width)
                           v = 1 - (fromIntegral y) / (fromIntegral height)
                           ray = Ray.Ray (P.Point 0 0 0) (D.direction (u-0.5) (v-0.5) 1)
                           (incoming, _) = surface_integrator ray (randoms $ mkStdGen (i*1812+x+y*77))
                           (sR, sG, sB) = from_XYZ_to_sRGB . Spectrum.toXYZ $ incoming
                       in (RGB sR sG sB) `shrink` fromIntegral samples)
                      [0..samples]



ppm :: String
ppm = 
  let width  = 200
      height = 200
      primitive'  = primitiveList [
                     primitiveFromShape (sphere (P.Point (-1.0) 0.0 5) 1)
                                        (BSDF.bsdf [X.lambertian (spectrumFromRGB 400 800 8 (RGB 1 0.3 0.3))]),
                     primitiveFromShape (sphere (P.Point 1.0 0.0 5) 1)
                                        (BSDF.bsdf [--X.lambertian (spectrum 100 600 [1.0])
                                                   X.lambertian (spectrumFromRGB 400 800 8 (RGB 1 1 1))
                                                   ]),
                     primitiveFromShape (Plane.fromPointNormal (P.Point (0) (-1) 0) (N.normal 0 1 0))
                                        (BSDF.bsdf [X.lambertian (gray 400 800 8 1)])
                     --primitiveFromShape (Plane.fromPointNormal (P.Point (-1) (-1) 0) (N.normal 1 0 0)) 
                     --                   (BSDF.bsdf [X.lambertian (spectrumFromRGB 400 800 8 (RGB 1 1 1))])
                    ]
      lightSources = [--Directional (D.direction 0 0 0) (gray 400 800 8 3.4)
                      -- ,Directional (D.direction 0 1.0 0) (spectrumFromSPD 300 830 6 $ regularSPD 100 600 [3])]
                     ] :: [LightSource]

      background ray = Spectrum.stretch (Photometry.Background.Preetham.Preetham.preetham ray) 0.00018

      !primitive = primitive'
      integrator = path 7 primitive lightSources background

      pixels = raytrace width height integrator `using` parListChunk (512) rdeepseq
  
  in  toPPM width height pixels

main :: IO ()
main = putStrLn $ ppm
