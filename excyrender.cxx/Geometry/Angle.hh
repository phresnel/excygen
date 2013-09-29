// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef ANGLE_HH_INCLUDED_20130929
#define ANGLE_HH_INCLUDED_20130929

#include "real.hh"

namespace excyrender { namespace Geometry {

    struct Degrees {
        Degrees() = delete;
        explicit constexpr Degrees(real deg) noexcept : deg(deg) {}
        explicit constexpr operator real() noexcept { return deg; }
    private:
        real deg;
    };

    struct Radians {
        Radians() = delete;
        explicit constexpr Radians(real rad) noexcept : rad(rad) {}
        explicit constexpr operator real() noexcept { return rad; }
    private:
        real rad;
    };

    struct Angle {
        constexpr Angle(Degrees deg) noexcept : rad(static_cast<real>(deg) * pi / 180) {}
        constexpr Angle(Radians rad) noexcept : rad(rad) {}

        constexpr operator Degrees() noexcept { return Degrees{static_cast<real>(rad) * 180 / pi }; }
        constexpr operator Radians() noexcept { return rad; }
    private:
        Radians rad;
    };

} }

#endif // ANGLE_HH_INCLUDED_20130929
