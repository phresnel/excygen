-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.SPD.Constant
( constantSPD
) where

import SPD.SPD
import SPD.Regular (regularSPD)
import SPD.CIEMatchingCurves (cie_start, cie_end)


-- Constant ------------------------------------------------------------------------------------
constantSPD :: (RealFrac a) => a -> SPD a

constantSPD x = SPD {
                    sample = \_ -> x,
                    toXYZ  = toXYZ (regularSPD cie_start cie_end [x])
                }

