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

data Point t = Point t t t
               deriving (Show)

add  :: (Num t) => Point t -> V.Vector t -> Point  t
sub  :: (Num t) => Point t -> V.Vector t -> Point  t
diff :: (Num t) => Point t -> Point  t -> V.Vector t

add  (Point a b c) (V.Vector x y z) = Point  (a+x) (b+y) (c+z)
sub  (Point a b c) (V.Vector x y z) = Point  (a-x) (b-y) (c-z)
diff (Point a b c) (Point  x y z)   = V.Vector (a-x) (b-y) (c-z)

