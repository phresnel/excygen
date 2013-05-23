-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shapes.Plane(
    fromPointNormal
)where

import Shapes.Shape
import RealNum
import qualified Geometry.Point as P
import qualified Geometry.Vector as V
import qualified Geometry.Normal as N
import qualified Geometry.Ray as R
import qualified DifferentialGeometry as DG


-- Plane -----------------------------------------------------------------------
fromPointNormal :: P.Point -> N.Normal -> Shape



-- Impl ------------------------------------------------------------------------
data Hessian = Hessian N.Normal RealNum


fromPointNormal p n = 
 let plane = Hessian n $ -((P.asVector p) `V.dot` (N.asVector n))
 in Shape {
    intersect = isect plane,
    occludes = occl plane 
 }


isect :: Hessian -> R.Ray -> Maybe DG.DifferentialGeometry
isect plane@(Hessian n _) ray@(R.Ray orig dir) =
  let denom = n `N.dot'` dir
      p = signedDistance plane orig
      t = if denom==0 then 0 else -p/denom
      nn = if p >= 0 then n else N.neg n
  in if t<=0
     then Nothing
     else Just DG.DifferentialGeometry {
        DG.d = t,
        DG.poi = ray `R.point` t,
        DG.nn = nn,
        DG.u = 0, DG.v = 0,
        DG.dpdu = N.asVector $ N.createOrthogonal nn
     }

 
signedDistance :: Hessian -> P.Point -> RealNum
signedDistance (Hessian n d) p = (N.asVector n) `V.dot` (P.asVector p) + d


occl :: Hessian -> P.Point -> P.Point -> Bool
occl hess start end =
   let signA = 0 < signedDistance hess start 
       signB = 0 < signedDistance hess end
   in signA /= signB
