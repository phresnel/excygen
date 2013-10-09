// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef POINT2D_HH_INCLUDED_20130812
#define POINT2D_HH_INCLUDED_20130812

#include "real.hh"

namespace excyrender { namespace Geometry {

    struct Point2d final {
        real x=0, y=0;

        constexpr Point2d() noexcept = default;
        constexpr Point2d(real x, real y) noexcept : x(x), y(y) {}
    };

} }

#endif // POINT2D_HH_INCLUDED_20130812
