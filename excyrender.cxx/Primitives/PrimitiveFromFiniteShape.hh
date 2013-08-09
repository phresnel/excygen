// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PRIMTIVEFROMFINITESHAPE_HH_INCLUDED_20130722
#define PRIMTIVEFROMFINITESHAPE_HH_INCLUDED_20130722

#include "FinitePrimitive.hh"
#include "Shapes/FiniteShape.hh"

namespace excyrender { namespace Primitives {

class PrimitiveFromFiniteShape final : public FinitePrimitive
{
public:
    PrimitiveFromFiniteShape() = delete;
    PrimitiveFromFiniteShape(std::shared_ptr<const Shapes::FiniteShape> shape,
                             std::shared_ptr<const Photometry::Material::Material> const &material) :
        shape(shape),
        material(material)
    {
    }

    optional<Intersection> intersect(Geometry::Ray const &ray) const noexcept
    {
        if (const auto &dg = shape->intersect(ray)) {
            return Intersection{*dg, material};
        }
        return optional<Intersection>();
    }

    bool occludes(Geometry::Point const &a, Geometry::Point const &b) const noexcept
    {
        return shape->occludes(a,b);
    }

    bool occludes(Geometry::Point const &a, Geometry::Direction const &b) const noexcept
    {
        return shape->occludes(a,b);
    }

    AABB aabb () const noexcept
    {
        return shape->aabb();
    }

private:
    std::shared_ptr<const Shapes::FiniteShape> shape;
    std::shared_ptr<const Photometry::Material::Material> material;
};

} }

#endif // PRIMTIVEFROMFINITESHAPE_HH_INCLUDED_20130722
