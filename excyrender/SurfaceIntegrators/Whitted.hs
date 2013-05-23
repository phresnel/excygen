-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module SurfaceIntegrators.Whitted (
  whitted
) where


import Primitives.Primitive
import qualified Geometry.Point as P
import qualified Geometry.Direction as D 
import qualified Geometry.Normal as N
import qualified Geometry.Ray as Ray
import qualified Intersection as I
import qualified Photometry.BSDF.BSDF as BSDF
import qualified Photometry.BSDF.BxDF as BxDF
import qualified DifferentialGeometry as DG
import RealNum

import Photometry.SPD.Regular
import Photometry.Spectrum as Spectrum



data LightSource = Directional D.Direction Spectrum

lightSources :: [LightSource]
lightSources = [Directional (D.direction 1 1 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3])
                -- ,Directional (D.direction 0 1.0 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3])]
               ]

lightFrom :: D.Direction -> BSDF.BSDF -> Primitive -> P.Point -> N.Normal -> LightSource -> Spectrum
lightFrom wo bsdf primitive at n (Directional wi lSpec) =
    let pdf = BSDF.pdf bsdf wo wi
    in if pdf<=0 then spectrum 100 600 [0]
       else let s = if occludes primitive at (at `P.add` (wi `D.stretch` 1000000))
                    then 0
                    else 1
                dot = s * (max 0 $ n `N.dot'` wi)
                f   = (BSDF.f bsdf) wo wi
            in f `Spectrum.mul` (lSpec `Spectrum.stretch` (dot / pdf))


whitted :: Primitive -> Ray.Ray -> [RealNum] -> (Spectrum, [RealNum])
whitted = whitted_impl 14


whitted_impl :: Int -> Primitive -> Ray.Ray -> [RealNum] -> (Spectrum, [RealNum])
whitted_impl 0 _ _ randoms = (spectrum 100 600 [0], randoms)
whitted_impl depth primitive ray@(Ray.Ray _ direction) randoms =
    case intersect primitive ray of
        Just i -> let
                      poi_exact   = DG.poi $ I.differentialGeometry i
                      normal      = DG.nn $ I.differentialGeometry i
                      poi_outside = poi_exact `P.add` (normal `N.stretch` 0.0001)

                      bsdf = I.bsdf i
                      bsdfSpecRefl = (BSDF.sample_f bsdf) (Just BxDF.Specular) (Just BxDF.Reflective)

                      directLighting = foldr1 Spectrum.add . 
                                        map (lightFrom direction bsdf primitive poi_outside normal) $
                                        lightSources

                      (r_direction, r_spec, r_pdf) = bsdfSpecRefl (I.differentialGeometry i) (D.neg direction)
                      (r_incoming, randoms')       = (whitted_impl (depth-1) primitive (Ray.Ray poi_outside r_direction) randoms)
                      specularReflection = if r_pdf<=0 then spectrum 100 600 [0]
                                           else r_spec `Spectrum.mul` r_incoming `Spectrum.stretch` (1.0 / r_pdf)
                      specularTransmission = spectrum 100 600 [0]
                  in
                      (directLighting
                        `Spectrum.add` specularReflection
                        `Spectrum.add` specularTransmission
                      , randoms')
        Nothing -> (spectrum 100 600 [3], randoms)

