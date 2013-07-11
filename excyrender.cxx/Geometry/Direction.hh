// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef DIRECTION_HH_INCLUDED_20130709
#define DIRECTION_HH_INCLUDED_20130709

#include "Vector.hh"
#include <cassert>
#include <stdexcept>

namespace excyrender {
    namespace Geometry {
        struct Direction {
            const real x, y, z;

            Direction() = delete;

            constexpr Direction(real x, real y, real z) noexcept
            : x(ensure_normalized(x, excyrender::fabs(x*x + y*y + z*z)-1 <= 0.00001)),
              y(y), z(z)
            {
            }

            explicit operator Vector () const noexcept {
                return {x,y,z};
            }

        private:
            static constexpr real ensure_normalized(real x, bool c) {
                return c ? x :
                        throw std::runtime_error("|x*x+y*y+z*z| > 0.00001 in Direction(x,y,z)");
            }
        };

        constexpr inline real dot (Direction const &lhs, Direction const &rhs) noexcept {
            return lhs.x*rhs.x + lhs.y*rhs.y + lhs.z*rhs.z;
        }

        inline Direction direction(real x, real y, real z) noexcept {
            const auto l = std::sqrt(x*x + y*y + z*z);
            return {x/l, y/l, z/l};
        }

        inline Direction direction(Vector const &v) noexcept {
            return direction(v.x, v.y, v.z);
        }

        constexpr inline Vector operator* (Direction const &lhs, real f) noexcept {
            return {lhs.x*f, lhs.y*f, lhs.z*f};
        }

        constexpr inline Direction operator- (Direction const &v) noexcept {
            return {-v.x, -v.y, -v.z};
        }

        template <typename RNG>
        inline Direction cosineWeightedHemisphere (RNG &rng) noexcept {
            const auto x1 = rng();
            const auto x2 = rng();
            const auto phi = 2*pi*x1,
                       cosTheta = std::sqrt(x2),
                       sinTheta = std::sqrt(1-x2);
            return {std::cos(phi) * sinTheta,
                    cosTheta,
                    std::sin(phi) * sinTheta};
        }

        inline std::ostream& operator<< (std::ostream &os, Direction const &v) noexcept {
            return os << "direction{" << v.x << "," << v.y << "," << v.z << '}';
        }
    }
}

#endif
