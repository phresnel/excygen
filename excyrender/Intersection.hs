-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Intersection
( Intersection(..)
) where

import DifferentialGeometry
import Photometry.SPD.SPD
import Photometry.BSDF.BSDF

-- Intersection ----------------------------------------------------------------
data Intersection t = Intersection {
    differentialGeometry :: DifferentialGeometry t,
    bsdf :: BSDF t
} 

