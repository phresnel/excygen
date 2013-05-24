-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module SurfaceIntegrators.Path (
  path
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
import RealNum



data LightSource = Directional D.Direction Spectrum

lightSources :: [LightSource]
lightSources = [Directional (D.direction 1 1 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [7])
                -- ,Directional (D.direction 0 1.0 0) (spectrumFromSPD 100 600 1 $ regularSPD 100 600 [3])]
               ]

lightFrom :: D.Direction -> BSDF.BSDF -> Primitive -> P.Point -> N.Normal -> LightSource -> Spectrum
lightFrom wo bsdf primitive at n (Directional wi lightSpec) =
    let transmittance = if occludes primitive at (at `P.add` (wi `D.stretch` 1000000))
                        then 0
                        else 1
        dot = max 0 $ n `N.dot'` wi
        f   = BSDF.f bsdf wo wi
    in
        f `Spectrum.mul` lightSpec `Spectrum.stretch` (dot * transmittance)


path :: Primitive -> Ray.Ray -> [RealNum] -> (Spectrum, [RealNum])
path = path_impl 4


path_impl :: Int -> Primitive -> Ray.Ray -> [RealNum] -> (Spectrum, [RealNum])
path_impl 0 _ _ randoms = (spectrum 100 600 [2], randoms)
path_impl depth primitive ray@(Ray.Ray _ direction) randoms =
    case intersect primitive ray of
        Just i -> let
                      diffGeom = I.differentialGeometry i
                      normal   = DG.nn diffGeom
                      wo       = D.neg direction
                      bsdf     = I.bsdf i
                      sample_f = BSDF.sample_f bsdf Nothing Nothing

                      (poi_outside, poi_inside) = ((DG.poi diffGeom) `P.add` (normal `N.stretch` epsilon),
                                                   (DG.poi diffGeom) `P.sub` (normal `N.stretch` epsilon))

                      (wi, r_surf, r_pdf, randoms') = sample_f randoms diffGeom wo

                      directLighting = foldr Spectrum.add (spectrum 100 600 [0]) . 
                                        map (lightFrom wo bsdf primitive poi_outside normal) $
                                        lightSources

                      (r_incoming, randoms'') = path_impl (depth-1) primitive (Ray.Ray poi_outside wi) randoms'

                      reflection = if r_pdf<=0
                                   then spectrum 100 600 [0]
                                   else r_surf
                                     `Spectrum.mul`     r_incoming
                                     `Spectrum.stretch` (wi `D.dot` (N.asDirection normal) / r_pdf)
                  in 
                     (directLighting `Spectrum.add` reflection,
                      randoms'')
        Nothing -> (spectrum 100 600 [2], randoms)

