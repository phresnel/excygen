//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Copyright (C) 2010  Sebastian Mach (*1983)
// * mail: phresnel/at/gmail/dot/com
// * http://phresnel.org
// * http://picogen.org
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ifndef TURTLEVECTOR_HH_INCLUDED_20100805
#define TURTLEVECTOR_HH_INCLUDED_20100805

#include <cmath>

namespace xyto {

struct TurtleVector {
        double x,y,z;
        TurtleVector () : x(0), y(0), z(0) {}
        TurtleVector (double x, double y, double z) : x(x), y(y), z(z) {}

        TurtleVector& operator += (TurtleVector rhs) {
                x += rhs.x;
                y += rhs.y;
                z += rhs.z;
                return *this;
        }
};
inline TurtleVector operator+ (TurtleVector lhs, TurtleVector rhs) {
        return TurtleVector(lhs.x+rhs.x, lhs.y+rhs.y, lhs.z+rhs.z);
}
inline TurtleVector operator- (TurtleVector lhs, TurtleVector rhs) {
        return TurtleVector(lhs.x-rhs.x, lhs.y-rhs.y, lhs.z-rhs.z);
}
inline TurtleVector operator* (TurtleVector lhs, double rhs) {
        return TurtleVector(lhs.x*rhs, lhs.y*rhs, lhs.z*rhs);
}
inline TurtleVector operator* (double lhs, TurtleVector rhs) {
        return TurtleVector(lhs*rhs.x, lhs*rhs.y, lhs*rhs.z);
}

inline double dot (TurtleVector lhs, TurtleVector rhs) {
        return lhs.x*rhs.x + lhs.y*rhs.y + lhs.z*rhs.z;
}
inline double length_sq (TurtleVector vec) {
        return dot(vec, vec);
}
inline double length (TurtleVector vec) {
        return std::sqrt(length_sq(vec));
}
inline double distance (TurtleVector a, TurtleVector b) {
        return length(a-b);
}
inline TurtleVector normalize (TurtleVector vec) {
        const double len = 1/length(vec);
        return TurtleVector(vec.x*len,
                            vec.y*len,
                            vec.z*len);
}

inline TurtleVector cross (TurtleVector lhs, TurtleVector rhs) {
        return TurtleVector(
                lhs.y*rhs.z - lhs.z*rhs.y,
                lhs.z*rhs.x - lhs.x*rhs.z,
                lhs.x*rhs.y - lhs.y*rhs.x
        );
}

}
#endif // TURTLEVECTOR_HH_INCLUDED_20100805
