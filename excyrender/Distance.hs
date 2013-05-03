-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Distance (
  Distance, distance
) where

import RealNum

-- Distance --------------------------------------------------------------------
data Distance = Distance RealNum
                deriving (Show)

distance :: RealNum -> Distance

distance f | f>=0      = Distance f
           | otherwise = error "Distance can't be negative"

