// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "Terrain2d.hh"
#include "Shapes/Triangle.hh"

namespace excyrender { namespace Shapes {

Terrain2d::Terrain2d(Geometry::Rectangle const &target_,
                     Geometry::Rectangle const &source_,
                     int resolution,
                     Nature::HeightFunction height_)
{
    if (resolution <= 0)
        throw std::logic_error("Terrain2d: resolution must be >= 1");

    Shapes::BoundingIntervalHierarchyBuilder builder;

    const Geometry::Point2d
           sd {source_.width() / resolution, source_.height() / resolution },
           td {target_.width() / resolution, target_.height() / resolution };

    std::cerr << "tesselating terrain ..." << std::endl;
    for (int iz=0; iz!=resolution; ++iz) {
        const real v = iz / real(resolution);
        for (int ix=0; ix!=resolution; ++ix) {
            const real u = ix / real(resolution);

            const Geometry::Point2d source = source_(u,v),
                                    target = target_(u,v);
            const real Ah = height_(source.x,      source.y+sd.y),
                       Bh = height_(source.x+sd.x, source.y+sd.y),
                       Ch = height_(source.x,      source.y),
                       Dh = height_(source.x+sd.x, source.y);

            const Geometry::Point A {target.x,      Ah, target.y+td.y},
                                  B {target.x+td.x, Bh, target.y+td.y},
                                  C {target.x,      Ch, target.y},
                                  D {target.x+td.x, Dh, target.y};

            builder.add(shared_ptr<Triangle>(new Triangle(A,B,C)));
            builder.add(shared_ptr<Triangle>(new Triangle(C,B,D)));
        }
    }

    std::cerr << "building terrain bih ..." << std::endl;
    bih = builder.finalize(20);
}


optional<DifferentialGeometry> Terrain2d::intersect(Geometry::Ray const &ray) const noexcept
{
    return bih->intersect(ray);
}


bool Terrain2d::occludes(Geometry::Point const &A, Geometry::Point const &B) const noexcept
{
    return bih->occludes(A,B);
}


bool Terrain2d::occludes(Geometry::Point const &A, Geometry::Direction const &B) const noexcept
{
    return bih->occludes(A,B);
}


AABB Terrain2d::aabb() const noexcept
{
    return bih->aabb();
}


} }
