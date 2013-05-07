-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Intersection
( Intersection(..)
, distance
) where

import DifferentialGeometry
import Photometry.BSDF.BSDF
import RealNum

-- Intersection ----------------------------------------------------------------
data Intersection = Intersection {
    differentialGeometry :: DifferentialGeometry,
    bsdf :: BSDF
} 

distance :: Intersection -> RealNum



--------------------------------------------------------------------------------
distance = d . differentialGeometry
