-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shape
( Sphere(..)
) where

import Geometry(Point(..))

-- Sphere ----------------------------------------------------------------------
newtype Radius t = Radius t deriving(Show)

data Sphere t = Sphere (Point t) (Radius t)
              deriving(Show)

main = putStrLn $ show (Sphere (Point 1 1 1) (Radius 3))
