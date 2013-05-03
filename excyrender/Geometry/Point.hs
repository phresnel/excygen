-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Point (
  Point(..),
  Geometry.Point.add,
  Geometry.Point.sub,
  Geometry.Point.diff
) where

import Geometry.Vector as V
import RealNum

data Point = Point RealNum RealNum RealNum
             deriving (Show)

add  :: Point -> V.Vector -> Point
sub  :: Point -> V.Vector -> Point
diff :: Point -> Point    -> V.Vector

add  (Point a b c) (V.Vector x y z) = Point  (a+x) (b+y) (c+z)
sub  (Point a b c) (V.Vector x y z) = Point  (a-x) (b-y) (c-z)
diff (Point a b c) (Point  x y z)   = V.Vector (a-x) (b-y) (c-z)

