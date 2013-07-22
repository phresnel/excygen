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
