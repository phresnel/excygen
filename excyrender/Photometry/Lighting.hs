-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.Lighting (
  LightSource(..),
  directLighting
) where

import qualified DifferentialGeometry as DG
import qualified Geometry.Direction as D 
import qualified Geometry.Point as P
import qualified Geometry.Normal as N
import qualified Intersection as I
import qualified Photometry.BSDF.BSDF as BSDF
import Primitives.Primitive
import RealNum
import Photometry.Spectrum as Spectrum

data LightSource = Directional D.Direction Spectrum

lightFrom :: D.Direction -> BSDF.BSDF -> Primitive -> P.Point -> N.Normal -> LightSource -> Spectrum
lightFrom wo bsdf primitive at n (Directional wi lightSpec) =
    let transmittance = if occludes primitive at (at `P.add` (wi `D.stretch` 1000000))
                        then 0
                        else 1
        dot = max 0 $ n `N.dot'` wi
        f   = BSDF.f bsdf wo wi
    in
        f `Spectrum.mul` lightSpec `Spectrum.stretch` (dot * transmittance)

directLighting :: [LightSource] -> Primitive -> I.Intersection -> D.Direction -> Spectrum
directLighting lightSources primitive intersection wo =
  let
      diffGeom    = I.differentialGeometry intersection
      bsdf        = I.bsdf intersection
      poi_outside = (DG.poi diffGeom) `P.add` (normal `N.stretch` epsilon)
      normal      = DG.nn diffGeom
  in 
      foldr Spectrum.add (spectrum 100 600 [0]) . 
       map (lightFrom wo bsdf primitive poi_outside normal) $
       lightSources

