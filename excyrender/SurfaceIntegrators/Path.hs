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

import Photometry.Spectrum as Spectrum
import RealNum
import Photometry.Lighting as Lighting


path :: Int -> Primitive -> [LightSource] -> (Ray.Ray -> Spectrum) -> Ray.Ray -> [RealNum] -> (Spectrum, [RealNum])
path 0 _ _ _ _ randoms = (gray 400 800 6 0, randoms)
path depth primitive lightSources background ray@(Ray.Ray _ direction) randoms =
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

                      direct = Lighting.directLighting lightSources primitive i wo

                      (r_incoming, randoms'') = path (depth-1) primitive lightSources background (Ray.Ray poi_outside wi) randoms'

                      reflection = if r_pdf<=0
                                   then gray 400 800 6 0
                                   else r_surf
                                     `Spectrum.mul`     r_incoming
                                     `Spectrum.stretch` (wi `D.dot` (N.asDirection normal) / r_pdf)
                  in 
                     (direct `Spectrum.add` reflection,
                      randoms'')
        Nothing -> (background ray, randoms)

