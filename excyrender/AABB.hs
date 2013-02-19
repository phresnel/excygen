-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module AABB (
    aabb,
    left, right, top, bottom, front, back,
    min, max,
    width, height, depth, size
) where


import Geometry(Point(..))
import Prelude hiding(min,max)


-- AABB ------------------------------------------------------------------------
data AABB t = AABB (Point t) (Point t)
              deriving (Show)

aabb   :: (Ord t) => Point t -> Point t -> AABB t

left   :: AABB t -> t
right  :: AABB t -> t
top    :: AABB t -> t
bottom :: AABB t -> t
front  :: AABB t -> t
back   :: AABB t -> t

min    :: AABB t -> Point t
max    :: AABB t -> Point t

width  :: (Num t) => AABB t -> t
height :: (Num t) => AABB t -> t
depth  :: (Num t) => AABB t -> t
size   :: (Num t, Integral i) => AABB t -> i -> t -- Useful e.g. for kd-builds


-- impl ------------------------------------------------------------------------
aabb min@(Point a b c) max@(Point x y z) 
    | a>x || b>y || c>z = error "'aabb min max': Only valid for 'min<max'"
    | otherwise = AABB min max

left   (AABB (Point v _ _) _) = v
bottom (AABB (Point _ v _) _) = v
front  (AABB (Point _ _ v) _) = v
right  (AABB _ (Point v _ _)) = v
top    (AABB _ (Point _ v _)) = v
back   (AABB _ (Point _ _ v)) = v

min (AABB v _) = v
max (AABB _ v) = v

width  b = (right b) - (left b)
height b = (top b)   - (bottom b)
depth  b = (back b)  - (front b)
size box index | index==0 = width box
               | index==1 = height box
               | index==2 = depth box
               | otherwise = error "'size AABB i': 'i' must be 0, 1 or 2"

