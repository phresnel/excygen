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

            Vector() = default;
            Vector(real x, real y, real z) : x(x), y(y), z(z) {}
        };

        inline Vector operator+ (Vector const &lhs, Vector const &rhs) {
            return {lhs.x+rhs.x, lhs.y+rhs.y, lhs.z+rhs.z};
        }

        inline Vector operator- (Vector const &lhs, Vector const &rhs) {
            return {lhs.x-rhs.x, lhs.y-rhs.y, lhs.z-rhs.z};
        }

        inline real dot (Vector const &lhs, Vector const &rhs) {
            return lhs.x*rhs.x + lhs.y*rhs.y + lhs.z*rhs.z;
        }

        inline real len_sq (Vector const &v) {
            return dot(v,v);
        }

        inline real len (Vector const &v) {
            return std::sqrt(len_sq(v));
        }

        inline Vector operator* (Vector const &v, real f) {
            return {v.x*f, v.y*f, v.z*f};
        }

        inline Vector operator/ (Vector const &v, real f) {
            return {v.x/f, v.y/f, v.z/f};
        }

        inline Vector normalize (Vector const &v) {
            return v / len(v);
        }

        inline Vector cross (Vector const &lhs, Vector const &rhs) {
            return {lhs.y*rhs.z - lhs.z*rhs.y,
                    lhs.z*rhs.x - lhs.x*rhs.z,
                    lhs.x*rhs.y - lhs.y*rhs.x};
        }

        inline Vector createOrthogonal (Vector const &v) {
            const auto n = normalize(v);
            if (std::fabs(n.x) > std::fabs(n.y)) {
                const auto l = 1 / std::sqrt(n.x*n.x + n.z*n.z);
                return {-n.z*l, 0, n.x*l};
            } else {
                const auto l = 1 / std::sqrt(n.y*n.y + n.z*n.z);
                return {0, n.z*l, -n.y*l};
            }
        }

        inline std::ostream& operator<< (std::ostream &os, Vector const &v) {
            return os << "vector{" << v.x << "," << v.y << "," << v.z << '}';
        }
    }
}

#endif
