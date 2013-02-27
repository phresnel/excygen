-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module PPM (
  toPPM
) where

import RGB


-- PPM -------------------------------------------------------------------------
toPPM :: (RealFrac t) => Int -> Int -> [RGB t] -> String



---impl ------------------------------------------------------------------------
toPPM width height pixels =
  if width*height /= length pixels then
     error ("toPPM: width*height (" ++ show width ++ "*" ++ show height ++ "="
            ++ show (width*height) ++ ") does not equal"
            ++ " number of pixels (" ++ show (length pixels) ++ ")")
  else 
      "P3\n" ++
      show width ++ " " ++ show height ++ "\n" ++
      "255\n" ++
      showRowWise 0 pixels
  where     
      showRowWise w [] = "\n"
      showRowWise w (x:xs)
        | w==width-1  = printRGB x ++ "\n" ++ showRowWise 0 xs
        | otherwise   = printRGB x ++         showRowWise (w+1) xs
  
      printRGB rgb = show r ++ " " ++
                     show g ++ " " ++
                     show b ++ "  "
               where scaled = stretch rgb 255
                     (RGB r g b) = RGB.floor $ saturate scaled 0 255

