// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef SHAPE_HH_INCLUDED_20130712
#define SHAPE_HH_INCLUDED_20130712

#include "Geometry/Ray.hh"
#include "Geometry/Point.hh"
#include "DifferentialGeometry.hh"
#include "optional.hh"

namespace excyrender { namespace Shapes {

    class Shape {
    public:
        virtual ~Shape() {}

        virtual optional<DifferentialGeometry> intersect(Geometry::Ray const &) const noexcept = 0;
        virtual bool occludes(Geometry::Point const &, Geometry::Point const &) const noexcept = 0;
        virtual bool occludes(Geometry::Point const &, Geometry::Direction const &) const noexcept = 0;
    };

    // Free functions. These help make acceleration structures from the detail namespace more generic.
    inline optional<DifferentialGeometry> intersect(Shape const &p, Geometry::Ray const &r) noexcept
    {
        return p.intersect(r);
    }

    inline bool occludes(Shape const &p, Geometry::Point const &a, Geometry::Point const &b) noexcept
    {
        return p.occludes(a,b);
    }

    inline bool occludes(Shape const &p, Geometry::Point const &a, Geometry::Direction const &b) noexcept
    {
        return p.occludes(a,b);
    }


} }

#endif // SHAPE_HH_INCLUDED_20130712

