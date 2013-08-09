// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PRIMITIVEFROMSHAPE_HH_INCLUDED_20130718
#define PRIMITIVEFROMSHAPE_HH_INCLUDED_20130718

#include "Primitive.hh"
#include "Shapes/Shape.hh"
#include "Photometry/Material/Material.hh"

namespace excyrender { namespace Primitives {

class PrimitiveFromShape final : public Primitive
{
public:
    PrimitiveFromShape() = delete;
    PrimitiveFromShape(std::shared_ptr<const Shapes::Shape> shape,
                       std::shared_ptr<const Photometry::Material::Material> material) :
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
private:
    std::shared_ptr<const Shapes::Shape> shape;
    std::shared_ptr<const Photometry::Material::Material> material;
};

} }

#endif // PRIMITIVEFROMSHAPE_HH_INCLUDED_20130718
