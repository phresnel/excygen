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


data LightSource = Directional N.Normal Spectrum

lightSources :: [LightSource]
lightSources = [Directional (N.normal 1 0.0 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3]),
                Directional (N.normal 0 1.0 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3])]


lightFrom :: Primitive -> P.Point -> N.Normal -> LightSource -> Spectrum
lightFrom world at@(P.Point x y z) n (Directional dir spec) =  
        let s = occludes world at $
                   (at `P.add` (dir `N.stretch` 1000000)) 
            f = if s then 0 else max 0 $ n `N.dot` dir
        in (Spectrum.stretch spec) f


whitted :: Primitive -> Ray -> Spectrum
whitted primitive ray =
    case intersect primitive ray of
        Just i -> let
                      poi'   = DG.poi $ differentialGeometry i
                      normal = nn $ differentialGeometry i
                      poi    = poi' `P.add` (normal `N.stretch` 0.0001)

                      bsdf = Intersection.bsdf i
                      (f, pdf) = (BSDF.f bsdf, BSDF.pdf bsdf)

                      directLighting = foldr1 Spectrum.add . 
                                        map (lightFrom primitive poi normal) $
                                        lightSources
                      specularReflection   = spectrum 100 600 [0]
                      specularTransmission = spectrum 100 600 [0]
                  in
                      directLighting
                        `Spectrum.add` specularReflection
                        `Spectrum.add` specularTransmission
        Nothing -> spectrum 100 700 [0]

