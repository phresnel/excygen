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
            constexpr real x() const { return x_; }
            constexpr real y() const { return y_; }
            constexpr real z() const { return z_; }

            Direction() = delete;

            constexpr Direction(real x, real y, real z)
            : x_(ensure_normalized(x, excyrender::fabs(x*x + y*y + z*z)-1 <= 0.00001)),
              y_(y), z_(z)
            {
            }

            explicit operator Vector () const noexcept {
                return {x_,y_,z_};
            }
            
            
            friend constexpr inline real dot (Direction const &lhs, Direction const &rhs) noexcept {
                return lhs.x_*rhs.x_ + lhs.y_*rhs.y_ + lhs.z_*rhs.z_;
            }

            friend constexpr inline Vector operator* (Direction const &lhs, real f) noexcept {
                return {lhs.x_*f, lhs.y_*f, lhs.z_*f};
            }

            friend constexpr inline Vector operator* (real f, Direction const &lhs) noexcept {
                return {f*lhs.x_, f*lhs.y_, f*lhs.z_};
            }

            friend constexpr inline Direction operator- (Direction const &v) noexcept {
                // Note: Even though this uses a non-noexcept constructor,
                //       it never throws because it only negates an
                //       already constructed Normal.
                return {-v.x_, -v.y_, -v.z_};
            }

        private:
        
            const real x_, y_, z_;

            static constexpr real ensure_normalized(real x, bool c) {
                return c ? x :
                        throw std::runtime_error("|x*x+y*y+z*z| > 0.00001 in Direction(x,y,z)");
            }
        };
        
        inline Direction direction(real x, real y, real z) noexcept {
            const auto l = std::sqrt(x*x + y*y + z*z);
            return {x/l, y/l, z/l};
        }

        inline Direction direction(Vector const &v) noexcept {
            const auto l = std::sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
            return {v.x/l, v.y/l, v.z/l};
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
            return os << "direction{" << v.x() << "," << v.y() << "," << v.z() << '}';
        }
    }
}

#endif
