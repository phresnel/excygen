-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Direction (
  Direction, direction, 
  Geometry.Direction.stretch,
  u, v, w
) where

import Geometry.Vector as V
import RealNum

data Direction = Direction RealNum RealNum RealNum
                 deriving (Show)

direction :: RealNum -> RealNum -> RealNum -> Direction
stretch :: Direction -> RealNum -> V.Vector
u       :: Direction -> RealNum
v       :: Direction -> RealNum
w       :: Direction -> RealNum

direction a b c = 
    let direction_from_vec (V.Vector x y z) = Direction x y z
    in  direction_from_vec $ V.normalize $ V.Vector a b c

stretch (Direction a b c) f = V.Vector (a*f) (b*f) (c*f)
u (Direction f _ _) = f
v (Direction _ f _) = f
w (Direction _ _ f) = f


