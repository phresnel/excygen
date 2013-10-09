// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "factories.hh"
#include "PrimitiveFromFiniteShape.hh"
#include "Photometry/Material/Lambertian.hh"
#include "Photometry/Texture/ConstantTexture.hh"
#include "Shapes/Terrain2d.hh"
#include "Nature/HeightFunction.hh"

namespace excyrender { namespace Primitives {

std::shared_ptr<Primitives::FinitePrimitive> create_terrain2d_alpha (Geometry::Rectangle const &world_rect,
                                                                     int grid_resolution,
                                                                     Nature::HeightFunction height_function,
                                                                     std::shared_ptr<Photometry::Material::Material> material)
{
    if (grid_resolution <= 0)
        throw std::logic_error("called 'create_terrain2d_alpha' with negative grid_resolution");

    using Photometry::Spectrum;

    std::shared_ptr<Shapes::FiniteShape> shape (new Shapes::Terrain2d (world_rect,
                                                                       world_rect,
                                                                       grid_resolution,
                                                                       height_function));

    return std::shared_ptr<Primitives::FinitePrimitive>(new PrimitiveFromFiniteShape (shape, material));
}

} }
