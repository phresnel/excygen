// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "BoundingIntervalHierarchy.hh"

using namespace excyrender::Geometry;

namespace excyrender { namespace Primitives {

namespace {
}

optional<Intersection> BoundingIntervalHierarchy::intersect(Ray const &) const noexcept
{
    return optional<Intersection>();
}

bool BoundingIntervalHierarchy::occludes(Point const &a, Point const &b) const noexcept
{
    return false;
}

bool BoundingIntervalHierarchy::occludes(Point const &a, Direction const &b) const noexcept
{
    return false;
}

AABB BoundingIntervalHierarchy::aabb() const noexcept
{
    throw std::runtime_error("not implemented");
}

} }
