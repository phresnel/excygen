// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef SPHERE_HH_INCLUDED_20130712
#define SPHERE_HH_INCLUDED_20130712

#include "Shapes/Shape.hh"
#include "Geometry/Vector.hh"
#include "Geometry/Normal.hh"
#include <stdexcept>

namespace excyrender { namespace Shapes {

    class Sphere final : public Shape {
    public:
        Sphere (Geometry::Point const &center, real radius) : center(center), radius(radius)
        {
            if (radius<0) throw std::runtime_error("sphere radius must be positive");
        }
        
        optional<DifferentialGeometry> intersect(Geometry::Ray const &ray) const {
            using namespace Geometry;
            const Vector diff = ray.origin - center;
            const real d0 = dot(diff, static_cast<Vector>(ray.direction)),
                       d1 = d0 * d0,
                       d2 = dot(ray.direction, ray.direction),
                       d3 = len_sq(diff),
                       discriminant = d1 - d2*(d3 - radius*radius);
            if (discriminant<0) return optional<DifferentialGeometry>();
            
            const real solA = -d0 - sqrt(discriminant),
                       solB = -d0 + sqrt(discriminant);
                       
            if (solA > 0) {
                const real dd = solA / d2;
                const Point poi = ray(dd);
                const Vector p = poi - center;
                if (dd>epsilon)
                    return DifferentialGeometry{dd, poi, Normal::Normalize(p), 0, 0, Vector{-p.z, 0, p.x}};
            } else if (solB > 0) {
                const real dd = solB / d2;
                const Point poi = ray(dd);
                const Vector p = poi - center;
                if (dd>epsilon)
                    return DifferentialGeometry{dd, poi, Normal::Normalize(p), 0, 0, Vector{-p.z, 0, p.x}};
            }
            return optional<DifferentialGeometry>();
        }
        
        bool occludes(Geometry::Point const &start, Geometry::Point const &end) const {
            using namespace Geometry;
            const Vector diff = start - center,
                         direction = normalize(end-start);
            const real d0 = dot(diff, static_cast<Vector>(direction)),
                       d1 = d0 * d0,
                       d2 = dot(direction, direction),
                       d3 = len_sq(diff),
                       discriminant = d1 - d2*(d3 - radius*radius);
            if (discriminant<0) return false;            
            const real solA = -d0 - sqrt(discriminant),
                       solB = -d0 + sqrt(discriminant);                       
            return (solA/d2)>epsilon || (solB/d2)>epsilon;
        }
        
        bool occludes(Geometry::Point const &start, Geometry::Direction const &direction) const {
            using namespace Geometry;
            const Vector diff = start - center;
            const real d0 = dot(diff, static_cast<Vector>(direction)),
                       d1 = d0 * d0,
                       d2 = dot(direction, direction),
                       d3 = len_sq(diff),
                       discriminant = d1 - d2*(d3 - radius*radius);
            if (discriminant<0) return false;            
            const real solA = -d0 - sqrt(discriminant),
                       solB = -d0 + sqrt(discriminant);                       
            return (solA/d2)>epsilon || (solB/d2)>epsilon;
        }

    private:
        Geometry::Point center;
        real radius;
    };

} }

#endif // SPHERE_HH_INCLUDED_20130712

