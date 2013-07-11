// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef NORMAL_HH_INCLUDED_20130709
#define NORMAL_HH_INCLUDED_20130709

#include "Vector.hh"
#include "Direction.hh"

namespace excyrender {
    namespace Geometry {
        struct Normal {
            const real x, y, z;

            Normal() = delete;
            Normal(real x, real y, real z) : x(x), y(y), z(z) noexcept 
            {
                assert(excyrender::fabs(x*x + y*y + z*z)-1 < 0.00001);
            }

            explicit operator Vector () const noexcept {
                return {x,y,z};
            }

            explicit operator Direction () const noexcept {
                return {x,y,z};
            }
        };

        constexpr inline real dot (Normal const &lhs, Normal const &rhs) noexcept {
            return lhs.x*rhs.x + lhs.y*rhs.y + lhs.z*rhs.z;
        }

        inline Normal normal(real x, real y, real z) noexcept {
            const auto l = std::sqrt(x*x + y*y + z*z);
            return {x/l, y/l, z/l};
        }

        inline Normal normal(Vector const &v) noexcept {
            return normal(v.x, v.y, v.z);
        }

        constexpr inline Vector operator* (Normal const &lhs, real f) noexcept {
            return {lhs.x*f, lhs.y*f, lhs.z*f};
        }

        inline Normal operator- (Normal const &v) noexcept {
            return {-v.x, -v.y, -v.z};
        }

        template <typename RNG>
        inline Normal cosineWeightedHemisphere (RNG &rng) noexcept {
            const auto x1 = rng();
            const auto x2 = rng();
            const auto phi = 2*pi*x1,
                       cosTheta = std::sqrt(x2),
                       sinTheta = std::sqrt(1-x2);
            return {std::cos(phi) * sinTheta,
                    cosTheta,
                    std::sin(phi) * sinTheta};
        }

        inline std::ostream& operator<< (std::ostream &os, Normal const &v) noexcept {
            return os << "normal{" << v.x << "," << v.y << "," << v.z << '}';
        }
    }
}

#endif

