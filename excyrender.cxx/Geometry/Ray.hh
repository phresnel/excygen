// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef RAY_HH_INCLUDED_20130709
#define RAY_HH_INCLUDED_20130709

#include "Point.hh"
#include "Direction.hh"

namespace excyrender {
    namespace Geometry {
        struct Ray {
            Point origin;
            Direction direction;

            Point operator() (real f) const {
                assert(f>=0);
                return origin + direction * f;
            }
        };

        inline std::ostream& operator<< (std::ostream &os, Ray const &v) {
            return os << "ray{" << v.origin << "," << v.direction << '}';
        }
    }
}

#endif

