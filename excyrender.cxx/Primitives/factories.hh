// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef FACTORIES_HH_20131009
#define FACTORIES_HH_20131009

#include "FinitePrimitive.hh"
#include "Geometry/Rectangle.hh"
#include "Nature/HeightFunction.hh"

namespace excyrender { namespace Primitives {

    std::shared_ptr<Primitives::FinitePrimitive> create_terrain2d_alpha(
        Geometry::Rectangle const &world_rect,
        int grid_resolution,
        Nature::HeightFunction height_function,
        std::shared_ptr<Photometry::Material::Material> material
    );

} }

#endif // FACTORIES_HH_20131009
