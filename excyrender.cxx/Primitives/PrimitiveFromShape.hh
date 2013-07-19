// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PRIMITIVEFROMSHAPE_HH_INCLUDED_20130718
#define PRIMITIVEFROMSHAPE_HH_INCLUDED_20130718

namespace excyrender { namespace Primitives {


class PrimitiveFromShape final : public Primitive
{
public:
    PrimitiveFromShape() = delete;
    PrimitiveFromShape(std::shared_ptr<Shapes::Shape> shape,
                       Photometry::Surface::BSDF const &bsdf) :
        shape(shape),
        bsdf(bsdf)
    {
    }

    optional<Intersection> intersect(Geometry::Ray const &ray) const noexcept
    {
        if (const auto &dg = shape->intersect(ray)) {
            return Intersection{*dg, bsdf};
        }
        return optional<Intersection>();
    }
    
    bool occludes(Geometry::Point const &a, Geometry::Point const &b) const noexcept 
    {
        return shape->occludes(a,b);
    }
private:
    std::shared_ptr<Shapes::Shape> shape;
    Photometry::Surface::BSDF bsdf;
};



} }

#endif // PRIMITIVEFROMSHAPE_HH_INCLUDED_20130718
