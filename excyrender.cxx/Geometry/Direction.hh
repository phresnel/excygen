// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef DIRECTION_HH_INCLUDED_20130709
#define DIRECTION_HH_INCLUDED_20130709

#include "Vector.hh"
#include <cassert>

namespace excyrender {
    namespace Geometry {
        struct Direction {
            const real x, y, z;

            Direction() = delete;
            Direction(real x, real y, real z) : x(x), y(y), z(z)
            {
                assert(std::fabs(x*x + y*y + z*z)-1 < 0.00001);
            }

            explicit operator Vector () const {
                return {x,y,z};
            }
        };

        constexpr inline real dot (Direction const &lhs, Direction const &rhs) {
            return lhs.x*rhs.x + lhs.y*rhs.y + lhs.z*rhs.z;
        }

        inline Direction direction(real x, real y, real z) {
            const auto l = std::sqrt(x*x + y*y + z*z);
            return {x/l, y/l, z/l};
        }

        inline Direction direction(Vector const &v) {
            return direction(v.x, v.y, v.z);
        }

        constexpr inline Vector operator* (Direction const &lhs, real f) {
            return {lhs.x*f, lhs.y*f, lhs.z*f};
        }

        inline Direction operator- (Direction const &v) {
            return {-v.x, -v.y, -v.z};
        }

        template <typename RNG>
        inline Direction cosineWeightedHemisphere (RNG &rng) {
            const auto x1 = rng();
            const auto x2 = rng();
            const auto phi = 2*pi*x1,
                       cosTheta = std::sqrt(x2),
                       sinTheta = std::sqrt(1-x2);
            return {std::cos(phi) * sinTheta,
                    cosTheta,
                    std::sin(phi) * sinTheta};
        }

        inline std::ostream& operator<< (std::ostream &os, Direction const &v) {
            return os << "direction{" << v.x << "," << v.y << "," << v.z << '}';
        }
    }
}

#endif
