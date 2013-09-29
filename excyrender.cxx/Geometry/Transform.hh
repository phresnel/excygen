// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef TRANSFORM_HH_INCLUDED_20130929
#define TRANSFORM_HH_INCLUDED_20130929

#include "Matrix44.hh"
#include "Vector.hh"
#include "Point.hh"
#include "Normal.hh"
#include "Direction.hh"
#include "Ray.hh"
#include "AABB.hh"
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

        Point     operator* (const Point &)      const;
        Vector    operator* (const Vector &)     const;
        Normal    operator* (const Normal &)     const;
        Direction operator* (const Direction &)  const;
        Ray       operator* (const Ray &)        const;
        AABB      operator* (const AABB &)       const;

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



    inline Point Transform::operator* (const Point &pt) const {
        real x = pt.x, y = pt.y, z = pt.z;
        real xp = m(0,0)*x + m(0,1)*y + m(0,2)*z + m(0,3);
        real yp = m(1,0)*x + m(1,1)*y + m(1,2)*z + m(1,3);
        real zp = m(2,0)*x + m(2,1)*y + m(2,2)*z + m(2,3);
        real wp = m(3,0)*x + m(3,1)*y + m(3,2)*z + m(3,3);
        if (wp == 0) {
            throw std::logic_error("Transform::operator*(Point): wp == 0");
        }
        if (wp == real(1))
            return Point(xp, yp, zp);
        return Point(xp/wp, yp/wp, zp/wp);
    }
    inline Vector Transform::operator* (const Vector &v) const {
        real x = v.x, y = v.y, z = v.z;
        return Vector(m(0,0)*x + m(0,1)*y + m(0,2)*z,
                      m(1,0)*x + m(1,1)*y + m(1,2)*z,
                      m(2,0)*x + m(2,1)*y + m(2,2)*z);
    }
    inline Normal Transform::operator* (const Normal &n) const {
        real x = n.x(), y = n.y(), z = n.z();
        return Normal(inv(0,0)*x + inv(1,0)*y + inv(2,0)*z,
                      inv(0,1)*x + inv(1,1)*y + inv(2,1)*z,
                      inv(0,2)*x + inv(1,2)*y + inv(2,2)*z);
    }
    inline Direction Transform::operator* (const Direction &n) const {
        real x = n.x(), y = n.y(), z = n.z();
        return Direction(inv(0,0)*x + inv(1,0)*y + inv(2,0)*z,
                         inv(0,1)*x + inv(1,1)*y + inv(2,1)*z,
                         inv(0,2)*x + inv(1,2)*y + inv(2,2)*z);
    }
    inline Ray Transform::operator* (const Ray &ray) const {
        return Ray((*this)*ray.origin, (*this)*ray.direction);
    }
    inline AABB Transform::operator* (const AABB &b) const {
        const Transform &M = *this;
        AABB ret (M * Point(b.min().x, b.min().y, b.min().z));
        ret = union_(ret, M * Point(b.max().x, b.min().y, b.min().z));
        ret = union_(ret, M * Point(b.min().x, b.max().y, b.min().z));
        ret = union_(ret, M * Point(b.min().x, b.min().y, b.max().z));
        ret = union_(ret, M * Point(b.min().x, b.max().y, b.max().z));
        ret = union_(ret, M * Point(b.max().x, b.max().y, b.min().z));
        ret = union_(ret, M * Point(b.max().x, b.min().y, b.max().z));
        ret = union_(ret, M * Point(b.max().x, b.max().y, b.max().z));
        return ret;
    }

} }

#endif
