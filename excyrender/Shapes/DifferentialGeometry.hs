-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Shapes.DifferentialGeometry
( DifferentialGeometry(..)
) where

import Geometry(Point, Normal, ray_point)
import Distance


--------------------------------------------------------------------------------
data DifferentialGeometry t = DifferentialGeometry {
    d :: Distance t,
    poi :: Point t,
    nn :: Normal t,
    u :: t,
    v :: t
    -- shape :: Shape <-- This is as PBRT has it, but it would produce a 
    --                    recursive dependency
} deriving (Show)

