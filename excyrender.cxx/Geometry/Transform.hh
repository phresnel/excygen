// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef TRANSFORM_HH_INCLUDED_20130929
#define TRANSFORM_HH_INCLUDED_20130929

#include "Matrix44.hh"
#include "Vector.hh"
#include "Point.hh"
#include "Angle.hh"

namespace excyrender { namespace Geometry {

    struct Transform {
        constexpr Transform() noexcept = default;
        Transform(real t00, real t01, real t02, real t03,
                  real t10, real t11, real t12, real t13,
                  real t20, real t21, real t22, real t23,
                  real t30, real t31, real t32, real t33)
          : m{t00, t01, t02, t03,
              t10, t11, t12, t13,
              t20, t21, t22, t23,
              t30, t31, t32, t33},
            inv(inverse(m))
        {
        }

        Transform(Matrix44 const &m)
          : m(m), inv(inverse(m))
        {
        }

        static Transform Translate(const Vector &delta);
        static Transform Scale(real x, real y, real z);
        static Transform RotateX(Angle angle);
        static Transform RotateY(Angle angle);
        static Transform RotateZ(Angle angle);
        static Transform Rotate(Angle angle, const Vector &axis);
        static Transform LookAt(const Point &pos, const Point &look, const Vector &up);
        static Transform Orthographic(real znear, real zfar);
        static Transform Perspective(Angle fov, real n, real f);

        friend constexpr Transform inverse(Transform const &t) noexcept;
        friend constexpr Transform transpose(Transform const &t) noexcept;

        constexpr bool operator== (Transform const &rhs) noexcept {
            return (m == rhs.m) && (inv == rhs.inv);
        }

        constexpr bool operator!= (Transform const &rhs) noexcept {
            return !(*this == rhs);
        }

        Transform operator* (const Transform &t2) const noexcept {
            return Transform(m*t2.m, inv*t2.inv);
    }

    private:

        constexpr Transform(Matrix44 const &m, Matrix44 const &inv) noexcept : m(m), inv(inv) {}

        Matrix44 m, inv;
    };

    constexpr Transform inverse(Transform const &t) noexcept {
        return {t.inv, t.m};
    }

    constexpr Transform transpose(Transform const &t) noexcept {
        return {transpose(t.m), transpose(t.inv)};
    }

} }

#endif
