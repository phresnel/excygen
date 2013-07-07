-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.Background.Constant
( constant
) where

import Photometry.Spectrum as Spectrum
import Geometry.Ray as Ray

constant :: Spectrum -> (Ray.Ray -> Spectrum)
constant spec _ = spec
