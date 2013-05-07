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
import Geometry.Ray as Ray
import Photometry.RGB
import Intersection
import Photometry.ColorSpace
import Photometry.SPD.SPD as SPD
import Photometry.BSDF.BSDF as BSDF
import DifferentialGeometry as DG

import Photometry.SPD.Regular
import Photometry.SPD.Constant
import Photometry.Spectrum as Spectrum
import qualified Photometry.RGB as RGB

import RealNum


data LightSource = Directional D.Direction Spectrum

lightSources :: [LightSource]
lightSources = [Directional (D.direction 1 0.0 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3]),
                Directional (D.direction 0 1.0 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3])]


lightFrom :: D.Direction -> BSDF -> Primitive -> P.Point -> N.Normal -> LightSource -> Spectrum
lightFrom wo bsdf primitive at@(P.Point x y z) n (Directional wi lSpec) =
        let s = if occludes primitive at (at `P.add` (wi `D.stretch` 1000000))
                then 0
                else 1    
            dot = max 0 $ n `N.dot'` wi
            f   = (BSDF.f bsdf) wo wi

        in f `Spectrum.mul` (Spectrum.stretch lSpec dot)


whitted :: Primitive -> Ray -> Spectrum
whitted = whitted_impl 14


whitted_impl :: Int -> Primitive -> Ray -> Spectrum
whitted_impl 0 _ _ = spectrum 100 600 [0]
whitted_impl depth primitive ray@(Ray _ direction) =
    case intersect primitive ray of
        Just i -> let
                      poi'   = DG.poi $ differentialGeometry i
                      normal = nn $ differentialGeometry i
                      poi    = poi' `P.add` (normal `N.stretch` 0.0001)

                      bsdf = Intersection.bsdf i
                      (f, pdf) = (BSDF.f bsdf, BSDF.pdf bsdf)

                      directLighting = foldr1 Spectrum.add . 
                                        map (lightFrom (Ray.direction ray) bsdf primitive poi normal) $
                                        lightSources
                                        
                      (r_d, r_pdf) = (BSDF.sample_f bsdf) direction normal
                      specularReflection   = (whitted_impl (depth-1) primitive (Ray poi r_d))
                                               `Spectrum.stretch` (0.7 * 1 / r_pdf)   
                      specularTransmission = spectrum 100 600 [0]
                  in
                      directLighting
                        `Spectrum.add` specularReflection
                        `Spectrum.add` specularTransmission
        Nothing -> spectrum 100 600 [3]

