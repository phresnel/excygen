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
            constexpr real x() const { return x_; }
            constexpr real y() const { return y_; }
            constexpr real z() const { return z_; }

            Normal() = delete;

            constexpr Normal(real x, real y, real z)
            : x_(ensure_normalized(x, excyrender::fabs(x*x + y*y + z*z)-1 <= 0.00001)),
              y_(y), z_(z)
            {
            }

            static Normal Normalize(Vector v) noexcept {
                v = normalize(v);
                return {v.x, v.y, v.z};
            }

            explicit constexpr operator Vector () const noexcept {
                return {x_,y_,z_};
            }

            explicit constexpr operator Direction () const noexcept {
                return {x_,y_,z_};
            }

            explicit constexpr Normal(Direction const &dir) : x_(dir.x()), y_(dir.y()), z_(dir.z())
            {
            }



            friend constexpr inline real dot (Normal const &lhs, Normal const &rhs) noexcept {
                return lhs.x_*rhs.x_ + lhs.y_*rhs.y_ + lhs.z_*rhs.z_;
            }

            friend constexpr inline Vector operator* (Normal const &lhs, real f) noexcept {
                return {lhs.x_*f, lhs.y_*f, lhs.z_*f};
            }

            friend constexpr inline Vector operator* (real f, Normal const &lhs) noexcept {
                return {f*lhs.x_, f*lhs.y_, f*lhs.z_};
            }

            friend constexpr inline Normal operator- (Normal const &v) noexcept {
                // Note: Even though this uses a non-noexcept constructor,
                //       it never throws because it only negates an
                //       already constructed Normal.
                return {-v.x_, -v.y_, -v.z_};
            }

            inline void swap(Normal &rhs) noexcept {
                excyrender::swap(x_, rhs.x_);
                excyrender::swap(y_, rhs.y_);
                excyrender::swap(z_, rhs.z_);
            }

        private:
            real x_, y_, z_;

            static constexpr real ensure_normalized(real x, bool c) {
                return c ? x :
                        throw std::runtime_error("|x*x+y*y+z*z| > 0.00001 in Normal(x,y,z)");
            }
        };

        inline void swap (Normal &lhs, Normal &rhs) noexcept {
            lhs.swap(rhs);
        }

        inline Normal normal(real x, real y, real z) noexcept {
            const auto l = std::sqrt(x*x + y*y + z*z);
            return {x/l, y/l, z/l};
        }

        inline Normal normal(Vector const &v) noexcept {
            const auto l = std::sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
            return {v.x/l, v.y/l, v.z/l};
        }


        inline std::ostream& operator<< (std::ostream &os, Normal const &v) noexcept {
            return os << "normal{" << v.x() << "," << v.y() << "," << v.z() << '}';
        }
    }
}

#endif

