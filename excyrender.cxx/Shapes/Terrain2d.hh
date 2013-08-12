// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef TERRAIN2D_HH_INCLUDED_20130812
#define TERRAIN2D_HH_INCLUDED_20130812

#include "memory.hh"
#include "FiniteShape.hh"
#include "Geometry/Rectangle.hh"
#include "Shapes/BoundingIntervalHierarchy.hh"
#include <functional>

namespace excyrender { namespace Shapes {

    class Terrain2d final : public FiniteShape
    {
    public:
        Terrain2d() = delete;

        Terrain2d(Geometry::Rectangle const &target,
                  Geometry::Rectangle const &source,
                  int resolution,
                  std::function<real (real, real)>);

        optional<DifferentialGeometry> intersect(Geometry::Ray const &) const noexcept ;
        bool occludes(Geometry::Point const &, Geometry::Point const &) const noexcept ;
        bool occludes(Geometry::Point const &, Geometry::Direction const &) const noexcept ;
        AABB aabb() const noexcept ;

    private:
        shared_ptr<BoundingIntervalHierarchy> bih;
    };

} }

#endif // TERRAIN2D_HH_INCLUDED_20130812
