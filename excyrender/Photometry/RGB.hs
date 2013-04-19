-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

-- To avoid confusion (I am always confused with linear RGB, sRGB and all that), 
-- this module and the type RGB should be renamed, like e.g. ImagePixel or so.

module Photometry.RGB(
    RGB(..), saturate, stretch, shrink, Photometry.RGB.floor
)where

import Prelude as P

-- RGB -------------------------------------------------------------------------
data RGB t = RGB t t t
             deriving(Show)

saturate :: (Ord t)        => RGB t -> t -> t -> RGB t
stretch  :: (Fractional t) => RGB t -> t -> RGB t 
shrink   :: (Fractional t) => RGB t -> t -> RGB t
floor    :: (RealFrac a, Integral b) => RGB a -> RGB b


-- impl ------------------------------------------------------------------------
saturate (RGB r g b) min_ max_ = 
    let sat = min max_ . max min_
    in RGB (sat r) (sat g) (sat b)  

stretch (RGB r g b) f = RGB (r*f) (g*f) (b*f)
shrink  (RGB r g b) f = RGB (r/f) (g/f) (b/f)
floor   (RGB r g b)   = RGB (P.floor r) (P.floor g) (P.floor b)

