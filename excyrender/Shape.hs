-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shape
( Sphere(..)
) where

import Geometry(Point(..), Ray(..), direction)



-- Shape -----------------------------------------------------------------------
class Shape s where
    intersect :: (Floating t, Ord t) => Ray t -> s t -> Bool



-- Sphere ----------------------------------------------------------------------
data Sphere t = Sphere (Point t) t
                deriving(Show)

instance Shape Sphere where
    intersect ray sphere = False

