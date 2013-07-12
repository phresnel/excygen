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
        
        virtual optional<DifferentialGeometry> intersect(Geometry::Ray const &) const = 0;
        virtual bool occludes(Geometry::Point const &, Geometry::Point const &) const = 0;
    };

} }

#endif // SHAPE_HH_INCLUDED_20130712

