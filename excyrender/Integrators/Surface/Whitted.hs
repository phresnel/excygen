-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Integrators.Surface.Whitted (
  whitted
) where


import Primitives.Primitive
import qualified Geometry.Point as P
import qualified Geometry.Direction as D 
import qualified Geometry.Normal as N
import qualified Geometry.Ray as Ray
import qualified Intersection as I
import qualified Photometry.BSDF.BSDF as BSDF
import qualified DifferentialGeometry as DG

import Photometry.SPD.Regular
import Photometry.Spectrum as Spectrum



data LightSource = Directional D.Direction Spectrum

lightSources :: [LightSource]
lightSources = [Directional (D.direction 1 0.0 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3]),
                Directional (D.direction 0 1.0 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3])]


lightFrom :: D.Direction -> BSDF.BSDF -> Primitive -> P.Point -> N.Normal -> LightSource -> Spectrum
lightFrom wo bsdf primitive at n (Directional wi lSpec) =
        let s = if occludes primitive at (at `P.add` (wi `D.stretch` 1000000))
                then 0
                else 1    
            dot = s * (max 0 $ n `N.dot'` wi)
            f   = (BSDF.f bsdf) wo wi

        in f `Spectrum.mul` (Spectrum.stretch lSpec dot)


whitted :: Primitive -> Ray.Ray -> Spectrum
whitted = whitted_impl 14


whitted_impl :: Int -> Primitive -> Ray.Ray -> Spectrum
whitted_impl 0 _ _ = spectrum 100 600 [0]
whitted_impl depth primitive ray@(Ray.Ray _ direction) =
    case intersect primitive ray of
        Just i -> let
                      poi_exact   = DG.poi $ I.differentialGeometry i
                      normal      = DG.nn $ I.differentialGeometry i
                      poi_outside = poi_exact `P.add` (normal `N.stretch` 0.0001)

                      bsdf = I.bsdf i

                      directLighting = foldr1 Spectrum.add . 
                                        map (lightFrom (Ray.direction ray) bsdf primitive poi_outside normal) $
                                        lightSources
                                        
                      (r_d, r_pdf) = (BSDF.sample_f bsdf) direction normal
                      specularReflection   = (whitted_impl (depth-1) primitive (Ray.Ray poi_outside r_d))
                                               `Spectrum.stretch` (0.7 * 1 / r_pdf)   
                      specularTransmission = spectrum 100 600 [0]
                  in
                      directLighting
                        `Spectrum.add` specularReflection
                        `Spectrum.add` specularTransmission
        Nothing -> spectrum 100 600 [3]

