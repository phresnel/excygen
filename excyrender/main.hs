-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

import qualified Geometry.Ray as Ray
import qualified Geometry.Direction as D
import qualified Geometry.Point as P

import ImageFormat.PPM(toPPM)

import Photometry.RGB
import qualified Photometry.BSDF.BSDF as BSDF
import qualified Photometry.BSDF.BxDF as X
import Photometry.ColorSpace
import Photometry.Spectrum as Spectrum

import Shapes.Sphere

import Primitives.Primitive
import Primitives.PrimitiveFromShape
import Primitives.PrimitiveList

import Integrators.Surface.Whitted

import Control.Parallel.Strategies

--data Radiance t = Radiance t t t
--radianceFromRGB :: RGB t -> Radiance t
--radianceFromRGB (RGB r g b) = Radiance r g b 
--radianceToRGB :: Radiance t -> RGB t
--radianceToRGB (Radiance r g b) = RGB r g b


-- simple renderer -------------------------------------------------------------
raytrace :: Int -> Int -> Primitive -> (Primitive -> Ray.Ray -> Spectrum) -> [RGB]
raytrace width height primitive surface_integrator =
    --[trace_pixel x y 
    -- | y<-[0..height-1]
    -- , x<-[0..width-1]]
    --let trace_row y = map (trace_pixel y) [0..width-1]
    --    rows = map trace_row [0..height-1]
    --    flattened = concat rows
    --in flattened
    let raw = map trace_pixel [0..(width*height)-1]
    in (parMap rdeepseq) (\(r,g,b) -> RGB r g b) raw
    where trace_pixel p =             
            let u = fromIntegral (p `mod` width) / fromIntegral width
                v = 1 - fromIntegral (p `div` width) / fromIntegral height
                ray = Ray.Ray (P.Point 0 0 0) (D.direction (u-0.5) (v-0.5) 1)
                (sR, sG, sB) = from_XYZ_to_sRGB . Spectrum.toXYZ $ surface_integrator primitive ray
            in (sR,sG,sB)



ppm :: String
ppm = 
  let width  = 128
      height = 128
      primitive  = primitiveList [
                     primitiveFromShape (sphere (P.Point (-1.0) 0.0 5) 1)
                                        (BSDF.bsdf [X.lambertian (spectrum 100 600 [1])]),
                     primitiveFromShape (sphere (P.Point 1.0 0.5 5) 1)
                                        (BSDF.bsdf [X.lambertian (spectrum 100 600 [0.75]),
                                                    X.specularReflect (spectrum 100 600 [0.25])
                                                   ])
                   ]
      pixels = raytrace width height primitive whitted
  
  in  toPPM width height pixels

main :: IO ()
main = putStrLn $ ppm
