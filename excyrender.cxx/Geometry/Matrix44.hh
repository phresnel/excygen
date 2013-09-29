// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef MATRIX44_HH_INCLUDED_20130929
#define MATRIX44_HH_INCLUDED_20130929

#include "real.hh"

namespace excyrender { namespace Geometry {

    struct Matrix44 {

        constexpr Matrix44() noexcept = default;
        constexpr Matrix44(real t00, real t01, real t02, real t03,
                           real t10, real t11, real t12, real t13,
                           real t20, real t21, real t22, real t23,
                           real t30, real t31, real t32, real t33)
          : m{t00, t01, t02, t03,
              t10, t11, t12, t13,
              t20, t21, t22, t23,
              t30, t31, t32, t33}
        {
        }

        constexpr real operator() (int y, int x) noexcept {
            return m[y*4+x];
        }

        constexpr bool operator== (Matrix44 const &rhs) noexcept {
            return ((m[0]==rhs.m[0])   & (m[1]==rhs.m[1])   & (m[2]==rhs.m[2])   & (m[3]==rhs.m[3]))
                && ((m[4]==rhs.m[4])   & (m[5]==rhs.m[5])   & (m[6]==rhs.m[6])   & (m[7]==rhs.m[7]))
                && ((m[8]==rhs.m[8])   & (m[9]==rhs.m[9])   & (m[10]==rhs.m[10]) & (m[11]==rhs.m[11]))
                && ((m[12]==rhs.m[12]) & (m[13]==rhs.m[13]) & (m[14]==rhs.m[14]) & (m[15]==rhs.m[15]));
        }

        constexpr bool operator!= (Matrix44 const &rhs) noexcept {
            return !(*this == rhs);
        }

        Matrix44 operator* (Matrix44 const &rhs) noexcept {
            Matrix44 r;
            for (int i = 0; i < 4; ++i) {
                for (int j = 0; j < 4; ++j) {
                    r.m[i*4+j] = (*this)(i,0) * rhs(0,j) +
                                 (*this)(i,1) * rhs(1,j) +
                                 (*this)(i,2) * rhs(2,j) +
                                 (*this)(i,3) * rhs(3,j);
                }
            }
            return r;
        }

    private:
        real m[4*4] {
            1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0,
            0, 0, 0, 1
        };

        friend Matrix44 inverse(const Matrix44 &);
        friend constexpr Matrix44 transpose(const Matrix44 &m) noexcept;
    };

    Matrix44 inverse(Matrix44 const &m);

    constexpr Matrix44 transpose(const Matrix44 &m) noexcept {
        return {
            m.m[0*4+0], m.m[1*4+0], m.m[2*4+0], m.m[3*4+0],
            m.m[0*4+1], m.m[1*4+1], m.m[2*4+1], m.m[3*4+1],
            m.m[0*4+2], m.m[1*4+2], m.m[2*4+2], m.m[3*4+2],
            m.m[0*4+3], m.m[1*4+3], m.m[2*4+3], m.m[3*4+3]
        };
    }
} }

#endif
