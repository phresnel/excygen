-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.SPD.SPD (
  SPD(..)
) where

data SPD t = SPD {
    sample :: t -> t,
    toXYZ  :: (t, t, t),
    stretch :: t -> SPD t
}

