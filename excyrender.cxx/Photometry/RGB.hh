// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef RGB_HH_INCLUDED_20130708
#define RGB_HH_INCLUDED_20130708

#include "real.hh"

namespace excyrender {
    namespace Photometry {
        struct RGB {
            real r = 0, g = 0, b = 0;
            constexpr RGB(real r, real g, real b) : r(r), g(g), b(b) {}
        };

        constexpr
        RGB operator * (RGB const &rgb, real f) {
            return {rgb.r*f, rgb.g*f, rgb.b*f};
        }

        constexpr
        RGB saturate (RGB const &rgb, real min, real max) {
            return {excyrender::saturate(rgb.r, min, max),
                    excyrender::saturate(rgb.g, min, max),
                    excyrender::saturate(rgb.b, min, max)};
        }
    }
}


#endif // RGB_HH_INCLUDED_20130708

