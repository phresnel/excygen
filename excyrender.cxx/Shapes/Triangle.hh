// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef TRIANGLE_HH_INCLUDED_20130721
#define TRIANGLE_HH_INCLUDED_20130721

#include "Shapes/FiniteShape.hh"
#include "Geometry/Vector.hh"
#include "Geometry/Normal.hh"
#include "Geometry/Point.hh"
#include <stdexcept>

namespace excyrender { namespace Shapes {

    class Triangle final : public FiniteShape {
    public:
        Triangle (Geometry::Point const &A,
                  Geometry::Point const &B,
                  Geometry::Point const &C)
            : A(A), B(B), C(C)
            , normal(Geometry::Normal::Normalize(cross(B-A, C-A)))
        {
        }


        optional<DifferentialGeometry> intersect(Geometry::Ray const &ray) const noexcept {
            using namespace Geometry;
            real u, v;
            Vector dpdu;
            const auto t = intersect_(ray.origin, ray.direction, u, v, dpdu);
            if(t > epsilon) {
                return DifferentialGeometry(
                            t, ray(t),
                            dot(static_cast<Direction>(normal),ray.direction)>0?-normal:normal,
                            u, v, dpdu);
            }
            return optional<DifferentialGeometry>();
        }


        bool occludes(Geometry::Point const &start, Geometry::Point const &end) const noexcept {
            return intersect_(start, Geometry::Direction::Normalize(end-start));
        }


        bool occludes(Geometry::Point const &start, Geometry::Direction const &direction) const noexcept {
            return intersect_(start, direction) > epsilon;
        }

        AABB aabb() const noexcept {
            const auto u = minmax({A.x, B.x, C.x}),
                       v = minmax({A.y, B.y, C.y}),
                       w = minmax({A.z, B.z, C.z});
            return {Geometry::Point{u.first, v.first, w.first},
                    Geometry::Point{u.second, v.second, w.second}};
        }

        void swap (Triangle &rhs) noexcept {
            Geometry::swap (A, rhs.A);
            Geometry::swap (B, rhs.B);
            Geometry::swap (C, rhs.C);
            Geometry::swap (normal, rhs.normal);
        }

    private:
        Geometry::Point A, B, C;
        Geometry::Normal normal;

    private:

        // Möller-Trumbore-Implementation
        // (transcribed from http://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm)
        real intersect_(Geometry::Point const &start, Geometry::Direction const &direction) const noexcept {
            real unused1, unused2;
            Geometry::Vector unused3;
            return intersect_(start, direction, unused1, unused2, unused3);
        }

        real intersect_(Geometry::Point const &start, Geometry::Direction const &direction,
                        real &u, real &v, Geometry::Vector &e1) const noexcept
        {
            using Geometry::Vector;
            e1 = B-A;
            const auto e2 = C-A;
            const auto P = cross(static_cast<Vector>(direction), e2);
            const auto det = dot(e1, P);
            if(det > -epsilon && det < epsilon) return -1;
            const auto inv_det = 1 / det;
            const auto T = start - A;
            u = dot(T, P) * inv_det;
            if(u < 0 || u > 1) return -1;
            const auto Q = cross(T, e1);
            v = dot(static_cast<Vector>(direction), Q) * inv_det;
            if(v < 0 || u+v > 1) return -1;
            const auto t = dot(e2, Q) * inv_det;
            return t;
        }
    };



    inline void swap(Triangle &lhs, Triangle &rhs) noexcept {
        lhs.swap(rhs);
    }

    inline optional<DifferentialGeometry> intersect(Triangle const &p, Geometry::Ray const &r) noexcept
    {
        return p.intersect(r);
    }

    inline bool occludes(Triangle const &p, Geometry::Point const &a, Geometry::Point const &b) noexcept
    {
        return p.occludes(a,b);
    }

    inline bool occludes(Triangle const &p, Geometry::Point const &a, Geometry::Direction const &b) noexcept
    {
        return p.occludes(a,b);
    }

} }

#endif // TRIANGLE_HH_INCLUDED_20130721
