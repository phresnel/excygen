-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module ImageFormat.PPM (
  toPPM
) where

import Photometry.RGB


-- PPM -------------------------------------------------------------------------
toPPM :: Int -> Int -> [RGB] -> String



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
      showRowWise _ [] = "\n"
      showRowWise w (x:xs)
        | w==width-1  = printRGB x ++ "\n" ++ showRowWise 0 xs
        | otherwise   = printRGB x ++         showRowWise (w+1) xs
  
      printRGB rgb = show ir ++ " " ++
                     show ig ++ " " ++
                     show ib ++ "  "
               where scaled = stretch rgb 255.0
                     (RGB r g b) = (saturate scaled 0.0 255.0)
                     (ir, ig, ib) = (Prelude.floor r :: Int, 
                                     Prelude.floor g :: Int,
                                     Prelude.floor b :: Int)

