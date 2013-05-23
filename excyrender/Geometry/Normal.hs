-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Normal (
  Normal,
  normal,
  stretch, 
  asVector, asDirection,
  dot, dot',
  u, v, w,
  neg,
  createOrthogonal
) where

import qualified Geometry.Vector as V
import qualified Geometry.Direction as D
import RealNum

data Normal = Normal RealNum RealNum RealNum
              deriving (Show)

normal  :: RealNum -> RealNum -> RealNum -> Normal
stretch :: Normal -> RealNum -> V.Vector
asVector:: Normal -> V.Vector
asDirection:: Normal -> D.Direction
dot     :: Normal -> Normal -> RealNum
dot'    :: Normal -> D.Direction -> RealNum
u       :: Normal -> RealNum
v       :: Normal -> RealNum
w       :: Normal -> RealNum
neg     :: Normal -> Normal
createOrthogonal :: Normal -> Normal

normal a b c = 
    let normal_from_vec (V.Vector x y z) = Normal x y z
    in  normal_from_vec $ V.normalize $ V.Vector a b c

stretch (Normal x y z) = V.stretch (V.Vector x y z)
asVector (Normal x y z) = V.Vector x y z
asDirection (Normal x y z) = D.direction x y z

dot (Normal a b c) (Normal x y z) = a*x + b*y + c*z
dot' a b = dot a (Normal (D.u b) (D.v b) (D.w b))
u (Normal f _ _) = f
v (Normal _ f _) = f
w (Normal _ _ f) = f

neg (Normal a b c) = Normal (-a) (-b) (-c)

createOrthogonal (Normal x y z) =
     if abs x > abs y
     then let l = 1.0 / sqrt (x*x+z*z)
          in Normal (-z*l) 0.0 (x*l)
     else let l = 1.0 / sqrt (y*y+z*z)
          in Normal 0.0 (z*l) (-y*l)

