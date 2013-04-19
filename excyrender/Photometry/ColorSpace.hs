-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.ColorSpace
where


from_XYZ_to_sRGB :: (RealFrac a) => (a, a, a) -> (a, a, a)


from_XYZ_to_sRGB (x, y, z) =
    ( 3.240479*x - 1.537150*y - 0.498535*z,
     -0.969256*x + 1.875991*y + 0.041556*z,
      0.055648*x - 0.204043*y + 1.057311*z)
    -- see: http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    --      https://github.com/mmp/pbrt-v2/blob/master/src/core/spectrum.h#L51
