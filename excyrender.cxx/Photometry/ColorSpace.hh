// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef COLORSPACE_HH_INCLUDED_20130712
#define COLORSPACE_HH_INCLUDED_20130712

#include "real.hh"

namespace excyrender { namespace Photometry { namespace ColorSpace {
    inline
    triple<real> XYZ_to_sRGB(triple<real> const &c) {
        return make_triple(3.240479*get<0>(c) - 1.537150*get<1>(c) - 0.498535*get<2>(c),
                          -0.969256*get<0>(c) + 1.875991*get<1>(c) + 0.041556*get<2>(c),
                           0.055648*get<0>(c) - 0.204043*get<1>(c) + 1.057311*get<2>(c));
        // see: http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
        //      https://github.com/mmp/pbrt-v2/blob/master/src/core/spectrum.h#L51
    }
} } }

#endif // COLORSPACE_HH_INCLUDED_20130712

