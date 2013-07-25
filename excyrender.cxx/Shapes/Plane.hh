// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PLANE_HH_INCLUDED_20130712
#define PLANE_HH_INCLUDED_20130712

#include "Shapes/Shape.hh"
#include "Geometry/Vector.hh"
#include "Geometry/Normal.hh"

namespace excyrender { namespace Shapes {

    class Plane final : public Shape {
    public:
        static Plane FromPointNormal(Geometry::Point const &p, Geometry::Normal const &n) noexcept
        {
            using Geometry::Vector;
            return {n, -(dot(Vector{p.x,p.y,p.z}, Vector{n.x(),n.y(),n.z()}))};
        }

        optional<DifferentialGeometry> intersect(Geometry::Ray const &ray) const noexcept
        {
            using namespace Geometry;

            const real denom = dot(static_cast<Direction>(n), ray.direction),
                       p = signedDistance(ray.origin),
                       t = denom==0? 0 : -p/denom;
            const Normal nn = p>=0 ? n : -n;

            if (t<=epsilon) return optional<DifferentialGeometry>();

            return DifferentialGeometry{t, ray(t), nn, 0, 0,
                                        createOrthogonal(static_cast<Vector>(nn))};
        }

        bool occludes(Geometry::Point const &start, Geometry::Point const &end) const noexcept {
            using namespace Geometry;

            const real denom = dot(static_cast<Direction>(n), direction(end-start)),
                       p = signedDistance(start),
                       t = denom==0? 0 : -p/denom;
            return t>epsilon;
        }

        bool occludes(Geometry::Point const &start, Geometry::Direction const &d) const noexcept {
            using namespace Geometry;

            const real denom = dot(static_cast<Direction>(n), d),
                       p = signedDistance(start),
                       t = denom==0? 0 : -p/denom;
            return t>epsilon;
        }

    private:
        // Implements the Hessian normal form.
        Geometry::Normal n;
        real d;

        Plane (Geometry::Normal const &n, real d) : n(n), d(d) {}

        real signedDistance (Geometry::Point const &p) const noexcept {
            using Geometry::Vector;
            return dot(Vector{n.x(),n.y(),n.z()}, Vector{p.x,p.y,p.z}) + d;
        }
    };
} }

#endif // PLANE_HH_INCLUDED_20130712

