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

#ifndef TURTLEMATRIX_HH_INCLUDED_20100805
#define TURTLEMATRIX_HH_INCLUDED_20100805

#include "turtlevector.hh"

namespace xyto {

class TurtleMatrix {
public:
        TurtleMatrix()  :
                m00(1), m01(0),  m02(0),
                m10(0), m11(1),  m12(0),
                m20(0), m21(0),  m22(1)
        {
        }

        TurtleMatrix(TurtleVector right, TurtleVector up, TurtleVector forw)
                :
                m00(right.x), m01(up.x),  m02(forw.x),
                m10(right.y), m11(up.y),  m12(forw.y),
                m20(right.z), m21(up.z),  m22(forw.z)
        {
        }

        TurtleMatrix (double m00, double m01, double m02,
                      double m10, double m11, double m12,
                      double m20, double m21, double m22
                      ) :
                m00(m00), m01(m01),  m02(m02),
                m10(m10), m11(m11),  m12(m12),
                m20(m20), m21(m21),  m22(m22)
        {
        }

        static TurtleMatrix RotateX(double a) {
                using std::sin; using std::cos;
                return TurtleMatrix(
                        1, 0,      0,
                        0, cos(a), -sin(a),
                        0, sin(a), cos(a)
                );
        }
        static TurtleMatrix RotateY(double a) {
                using std::sin; using std::cos;
                return TurtleMatrix(
                        cos(a), 0, -sin(a),
                        0,      1, 0,
                        sin(a), 0, cos(a)
                );
        }
        static TurtleMatrix RotateZ(double a) {
                using std::sin; using std::cos;
                return TurtleMatrix(
                        cos(a),  -sin(a), 0,
                        sin(a), cos(a), 0,
                        0,       0,      1
                );
        }

        static TurtleMatrix Rotate (double angle, TurtleVector axis) {
                using std::sin; using std::cos;
                const TurtleVector a = normalize(axis);
                const double s = sin(angle), c = cos(angle);

                return TurtleMatrix (
                        a.x * a.x + (1.f - a.x * a.x) * c,
                        a.x * a.y * (1.f - c) - a.z * s,
                        a.x * a.z * (1.f - c) + a.y * s,

                        a.x * a.y * (1.f - c) + a.z * s,
                        a.y * a.y + (1.f - a.y * a.y) * c,
                        a.y * a.z * (1.f - c) - a.x * s,

                        a.x * a.z * (1.f - c) - a.y * s,
                        a.y * a.z * (1.f - c) + a.x * s,
                        a.z * a.z + (1.f - a.z * a.z) * c
                );
        }


        TurtleMatrix operator * (TurtleMatrix rhs) const {
                TurtleMatrix ret;
                for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) {
                        ret[i+j*3] =
                                (*this)[i + 0*3] * rhs[0 + 3*j] +
                                (*this)[i + 1*3] * rhs[1 + 3*j] +
                                (*this)[i + 2*3] * rhs[2 + 3*j]
                        ;
                }
                return ret;
        }

        TurtleVector operator * (TurtleVector v) const {
                return TurtleVector(
                        m00*v.x + m01*v.y + m02*v.z,
                        m10*v.x + m11*v.y + m12*v.z,
                        m20*v.x + m21*v.y + m22*v.z
                );
        }


        TurtleVector disk_normal (double phi) {
                return normalize( up() * cos(phi)
                                + right() * -sin(phi));
        }

        TurtleVector right() const { return TurtleVector(m00,m10,m20); }
        TurtleVector up() const { return TurtleVector(m01,m11,m21); }
        TurtleVector forward() const { return TurtleVector(m02,m12,m22); }

private:
        double& operator [] (unsigned int index) {
                switch (index) {
                default:
                case 0*3+0: return m00;
                case 0*3+1: return m01;
                case 0*3+2: return m02;
                case 1*3+0: return m10;
                case 1*3+1: return m11;
                case 1*3+2: return m12;
                case 2*3+0: return m20;
                case 2*3+1: return m21;
                case 2*3+2: return m22;
                }
        }
        double operator [] (unsigned int index) const {
                switch (index) {
                default:
                case 0*3+0: return m00;
                case 0*3+1: return m01;
                case 0*3+2: return m02;
                case 1*3+0: return m10;
                case 1*3+1: return m11;
                case 1*3+2: return m12;
                case 2*3+0: return m20;
                case 2*3+1: return m21;
                case 2*3+2: return m22;
                }
        }

        double m00, m01, m02;
        double m10, m11, m12;
        double m20, m21, m22;
};

}

#endif // TURTLEMATRIX_HH_INCLUDED_20100805
