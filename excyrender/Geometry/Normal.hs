-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Normal (
  Normal,
  normal,
  stretch, asVector,
  dot, dot',
  u, v, w,
  neg
) where

import qualified Geometry.Vector as V
import qualified Geometry.Direction as D
import RealNum

data Normal = Normal RealNum RealNum RealNum
              deriving (Show)

normal  :: RealNum -> RealNum -> RealNum -> Normal
stretch :: Normal -> RealNum -> V.Vector
asVector:: Normal -> V.Vector
dot     :: Normal -> Normal -> RealNum
dot'    :: Normal -> D.Direction -> RealNum
u       :: Normal -> RealNum
v       :: Normal -> RealNum
w       :: Normal -> RealNum
neg     :: Normal -> Normal

normal a b c = 
    let normal_from_vec (V.Vector x y z) = Normal x y z
    in  normal_from_vec $ V.normalize $ V.Vector a b c

stretch (Normal x y z) = V.stretch (V.Vector x y z)
asVector (Normal x y z) = V.Vector x y z

dot (Normal a b c) (Normal x y z) = a*x + b*y + c*z
dot' a b = dot a (Normal (D.u b) (D.v b) (D.w b))
u (Normal f _ _) = f
v (Normal _ f _) = f
w (Normal _ _ f) = f

neg (Normal a b c) = Normal (-a) (-b) (-c)
