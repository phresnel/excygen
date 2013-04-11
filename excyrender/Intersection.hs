-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Intersection
( Intersection(..)
) where

import RGB
import Distance


-- Intersection ----------------------------------------------------------------
data Intersection t = Intersection {
    d :: Distance t,
    diffuse :: RGB t
} deriving (Show)

