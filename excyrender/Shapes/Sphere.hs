-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shapes.Sphere(
    sphere
)where

import Shapes.Shape
import qualified Geometry.Point as P
import qualified Geometry.Direction as D
import qualified Geometry.Normal as N
import qualified Geometry.Vector as V
import qualified Geometry.Ray as R
import qualified DifferentialGeometry as DG
import RealNum


-- Sphere ----------------------------------------------------------------------
sphere :: P.Point -> RealNum -> Shape



-- Impl ------------------------------------------------------------------------
sphere center radius 
    | radius<0  = error "sphere radius must be positive"
    | otherwise = Shape {
                      intersect = isectRaySphere center radius,
                      occludes = occl center radius
                  }


isectRaySphere :: P.Point -> RealNum -> R.Ray -> Maybe DG.DifferentialGeometry

isectRaySphere center radius ray =
  let
    (origin, direction) = ((R.origin ray), (R.direction ray))
    (V.Vector a b c) = origin `P.diff` center
    d0  = a*(D.u direction) + b*(D.v direction) + c*(D.w direction)
    d1  = d0**2
    d2  = (D.u direction)**2 + (D.v direction)**2 + (D.w direction)**2
    d3  = a**2 + b**2 + c**2
    discriminant = d1 - d2*(d3 - radius**2)
  in if discriminant<0 then Nothing
     else let
       solA = -d0 - (sqrt discriminant)
       solB = -d0 + (sqrt discriminant)
     in if solA>0 then
            let dd   = solA/d2
                poi' = R.point ray dd
            in Just DG.DifferentialGeometry {DG.d=dd,
                                             DG.poi=poi',
                                             DG.nn=let (V.Vector x y z) = poi' `P.diff` center
                                                   in N.normal x y z,
                                             DG.u=0,
                                             DG.v=0 }
        else if solB>0 then
            let dd   = solB/d2
                poi' = R.point ray dd
            in Just DG.DifferentialGeometry {DG.d=dd,
                                             DG.poi=poi',
                                             DG.nn=let (V.Vector x y z) = poi' `P.diff` center
                                                   in N.normal x y z,
                                             DG.u=0,
                                             DG.v=0 }
        else Nothing



occl :: P.Point -> RealNum -> P.Point -> P.Point -> Bool

occl center radius origin target = 
  let     
    direction = D.direction u v w
                where (V.Vector u v w) = target `P.diff` origin 
    (V.Vector a b c) = origin `P.diff` center
    d0  = a*(D.u direction) + b*(D.v direction) + c*(D.w direction)
    d1  = d0**2
    d2  = (D.u direction)**2 + (D.v direction)**2 + (D.w direction)**2
    d3  = a**2 + b**2 + c**2
    discriminant = d1 - d2*(d3 - radius**2)    
  in if discriminant<0 then False
     else let
       solA = -d0 - (sqrt discriminant)
       solB = -d0 + (sqrt discriminant)
     in solA>0 || solB>0

