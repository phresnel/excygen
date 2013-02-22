-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module RGB(
    RGB, saturate, stretch, shrink
)where


-- RGB -------------------------------------------------------------------------
data RGB t = RGB t t t
             deriving(Show)

saturate :: (Ord t)        => RGB t -> t -> t -> RGB t
stretch  :: (Fractional t) => RGB t -> t -> RGB t 
shrink   :: (Fractional t) => RGB t -> t -> RGB t


-- impl ------------------------------------------------------------------------
saturate (RGB r g b) min_ max_ = 
    let sat = min max_ . max min_
    in RGB (sat r) (sat g) (sat b)  

stretch (RGB r g b) f = RGB (r*f) (g*f) (b*f)
shrink  (RGB r g b) f = RGB (r/f) (g/f) (b/f)
