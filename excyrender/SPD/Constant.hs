-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module SPD.Constant
( constantSPD,
  sample,
  toXYZ
) where

import SPD
import SPD.Regular (regularSPD)
import CIEMatchingCurves (cie_start, cie_end)


-- Constant ------------------------------------------------------------------------------------
data Constant t = Constant t
                  deriving (Show)

instance SPD Constant where
    sample (Constant x) _ = x 
    toXYZ  (Constant x)   = toXYZ (regularSPD cie_start cie_end [x])


constantSPD :: (RealFrac a) => a -> Constant a

constantSPD = Constant

