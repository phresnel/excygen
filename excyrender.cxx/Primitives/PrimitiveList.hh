// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PRIMITIVELIST_HH_INCLUDED_20130718
#define PRIMITIVELIST_HH_INCLUDED_20130718

#include "Primitives/Primitive.hh"
#include <vector>
#include <memory>

namespace excyrender { namespace Primitives {


class PrimitiveList final : public Primitive
{
public:
    PrimitiveList() = delete;
    PrimitiveList(std::initializer_list<std::shared_ptr<Primitive>> primitives) :
        primitives(primitives)
    {
    }

    optional<Intersection> intersect(Geometry::Ray const &ray) const noexcept
    {
        optional<Intersection> ret;
        for (auto const &prim : primitives) {
            if (auto i = prim->intersect(ray)) {
                if (!ret || distance(*i)<distance(*ret))
                    ret = i;
            }
        }
        return ret;
    }
    
    bool occludes(Geometry::Point const &a, Geometry::Point const &b) const noexcept 
    {
        for (auto const &prim : primitives)
            if (prim->occludes(a,b)) return true;
        return false;
    }
private:
    std::vector<std::shared_ptr<Primitive>> primitives;
};



} }

#endif // PRIMITIVELIST_HH_INCLUDED_20130718

