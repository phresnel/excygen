// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef VECTOR_HH_INCLUDED_20130709
#define VECTOR_HH_INCLUDED_20130709

#include "real.hh"
#include <cmath>
#include <ostream>

namespace excyrender {
    namespace Geometry {
        struct Vector {
            real x = 0, y = 0, z = 0;

            constexpr Vector() noexcept = default;
            constexpr Vector(real x, real y, real z) noexcept : x(x), y(y), z(z) {}
        };

        constexpr inline Vector operator+ (Vector const &lhs, Vector const &rhs) noexcept {
            return {lhs.x+rhs.x, lhs.y+rhs.y, lhs.z+rhs.z};
        }

        constexpr inline Vector operator- (Vector const &lhs, Vector const &rhs) noexcept {
            return {lhs.x-rhs.x, lhs.y-rhs.y, lhs.z-rhs.z};
        }

        constexpr inline real dot (Vector const &lhs, Vector const &rhs) noexcept {
            return lhs.x*rhs.x + lhs.y*rhs.y + lhs.z*rhs.z;
        }

        constexpr inline real len_sq (Vector const &v) noexcept {
            return dot(v,v);
        }

        constexpr inline real len (Vector const &v) noexcept {
            return std::sqrt(len_sq(v));
        }

        constexpr inline Vector operator* (Vector const &v, real f) noexcept {
            return {v.x*f, v.y*f, v.z*f};
        }

        constexpr inline Vector operator* (real f, Vector const &v) noexcept {
            return {f*v.x, f*v.y, f*v.z};
        }

        constexpr inline Vector operator/ (Vector const &v, real f) noexcept {
            return {v.x/f, v.y/f, v.z/f};
        }

        constexpr inline Vector normalize (Vector const &v) noexcept {
            return v / len(v);
        }

        constexpr inline Vector cross (Vector const &lhs, Vector const &rhs) noexcept {
            return {lhs.y*rhs.z - lhs.z*rhs.y,
                    lhs.z*rhs.x - lhs.x*rhs.z,
                    lhs.x*rhs.y - lhs.y*rhs.x};
        }

        inline Vector createOrthogonal (Vector const &v) noexcept {
            const auto n = normalize(v);
            if (std::fabs(n.x) > std::fabs(n.y)) {
                const auto l = 1 / std::sqrt(n.x*n.x + n.z*n.z);
                return {-n.z*l, 0, n.x*l};
            } else {
                const auto l = 1 / std::sqrt(n.y*n.y + n.z*n.z);
                return {0, n.z*l, -n.y*l};
            }
        }

        inline std::ostream& operator<< (std::ostream &os, Vector const &v) noexcept {
            return os << "vector{" << v.x << "," << v.y << "," << v.z << '}';
        }
    }
}

#endif
