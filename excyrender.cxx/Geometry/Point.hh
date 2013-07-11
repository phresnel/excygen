// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef POINT_HH_INCLUDED_20130709
#define POINT_HH_INCLUDED_20130709

#include "Vector.hh"

namespace excyrender {
    namespace Geometry {
        struct Point {
            real x = 0, y = 0, z = 0;

            constexpr Point() noexcept = default;
            constexpr Point(real x, real y, real z) noexcept : x(x), y(y), z(z) {}

            constexpr explicit operator Vector () const noexcept {
                return {x,y,z};
            }
        };

        constexpr inline Point operator+ (Point const &lhs, Vector const &rhs) noexcept {
            return {lhs.x+rhs.x, lhs.y+rhs.y, lhs.z+rhs.z};
        }

        constexpr inline Point operator- (Point const &lhs, Vector const &rhs) noexcept {
            return {lhs.x-rhs.x, lhs.y-rhs.y, lhs.z-rhs.z};
        }

        constexpr inline Vector operator- (Point const &lhs, Point const &rhs) noexcept {
            return {lhs.x-rhs.x, lhs.y-rhs.y, lhs.z-rhs.z};
        }

        inline std::ostream& operator<< (std::ostream &os, Point const &v) noexcept {
            return os << "point{" << v.x << "," << v.y << "," << v.z << '}';
        }
    }
}

#endif

