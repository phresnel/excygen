// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef POINT_HH_INCLUDED_20130709
#define POINT_HH_INCLUDED_20130709

#include "Vector.hh"
#include <stdexcept>

namespace excyrender {
    namespace Geometry {
        struct Point {
            real x = 0, y = 0, z = 0;

            constexpr Point() noexcept = default;
            constexpr Point(real x, real y, real z) noexcept : x(x), y(y), z(z) {}

            constexpr explicit Point(Geometry::Vector const &v) : Point(v.x,v.y,v.z) {}
            constexpr explicit operator Vector () const noexcept {
                return {x,y,z};
            }

            constexpr real operator[] (int i) const {
                return i==0 ? x :
                       i==1 ? y :
                       i==2 ? z :
                       throw std::logic_error("used index outside [0..2] to Point::operator[]");
            }
        };

        inline void swap(Point &lhs, Point &rhs) noexcept {
            excyrender::swap(lhs.x, rhs.x);
            excyrender::swap(lhs.y, rhs.y);
            excyrender::swap(lhs.z, rhs.z);
        }

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
            return os << "point{"
                      << std::fixed << v.x << ","
                      << std::fixed << v.y << ","
                      << std::fixed << v.z << '}';
        }
    }
}

#endif

