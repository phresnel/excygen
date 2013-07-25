// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PRIMITIVE_HH_INCLUDED_20130718
#define PRIMITIVE_HH_INCLUDED_20130718

#include "Geometry/Ray.hh"
#include "Intersection.hh"
#include "optional.hh"

namespace excyrender { namespace Primitives {

struct Primitive {
    virtual ~Primitive() {};
    virtual optional<Intersection> intersect(Geometry::Ray const &) const noexcept = 0;
    virtual bool occludes(Geometry::Point const &a, Geometry::Point const &b) const noexcept = 0;
    virtual bool occludes(Geometry::Point const &a, Geometry::Direction const &b) const noexcept = 0;
};


// Free functions. These help make acceleration structures from the detail namespace more generic.
inline optional<Intersection> intersect(Primitive const &p, Geometry::Ray const &r) noexcept
{
    return p.intersect(r);
}

inline bool occludes(Primitive const &p, Geometry::Point const &a, Geometry::Point const &b) noexcept
{
    return p.occludes(a,b);
}

inline bool occludes(Primitive const &p, Geometry::Point const &a, Geometry::Direction const &b) noexcept
{
    return p.occludes(a,b);
}


} }

#endif // PRIMITIVE_HH_INCLUDED_20130718

