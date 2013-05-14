-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

-- To avoid confusion (I am always confused with linear RGB, sRGB and all that), 
-- this module and the type RGB should be renamed, like e.g. ImagePixel or so.

module Photometry.RGB(
    RGB(..), 
    saturate,
    stretch, shrink,
    Photometry.RGB.floor,
    add
)where

import Prelude as P
import Control.DeepSeq
import RealNum

-- RGB -------------------------------------------------------------------------
data RGB = RGB Float Float Float
           deriving(Show)

--instance NFData RGB where
--    rnf (RGB r g b) = rnf r `seq` rnf g `seq` rnf b `seq` ()

saturate :: RGB -> RealNum -> RealNum -> RGB
stretch  :: RGB -> RealNum -> RGB 
shrink   :: RGB -> RealNum -> RGB
floor    :: RGB -> RGB
add      :: RGB -> RGB -> RGB


-- impl ------------------------------------------------------------------------
saturate (RGB r g b) min_ max_ = 
    let sat = min max_ . max min_
    in RGB (sat r) (sat g) (sat b)  

stretch (RGB r g b) f = RGB (r*f) (g*f) (b*f)
shrink  (RGB r g b) f = RGB (r/f) (g/f) (b/f)
floor   (RGB r g b)   = RGB (fromIntegral (P.floor r :: Int))
                            (fromIntegral (P.floor g :: Int))
                            (fromIntegral (P.floor b :: Int))

add (RGB a b c) (RGB x y z) = RGB (a+x) (b+y) (c+z)
