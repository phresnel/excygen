// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PLANARMAPPING2D_HH_20130811
#define PLANARMAPPING2D_HH_20130811

#include "DifferentialGeometry.hh"
#include "TexCoords2d.hh"
#include "Mapping2d.hh"
#include "memory.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    struct PlanarMapping2d final : Mapping2d {

        PlanarMapping2d (Geometry::Vector const &vs, Geometry::Vector const &vt,
                         real ds, real dt) noexcept
            : vs(vs), vt(vt), ds(ds), dt(dt)
        {
        }

        TexCoords2d operator() (DifferentialGeometry const &dg) const noexcept {
            const auto vec = dg.poi - Geometry::Point(0,0,0);
            return { ds + dot(vec, vs),
                     dt + dot(vec, vt) };
        }

    private:
        Geometry::Vector vs, vt;
        real ds, dt;
    };

    inline
    unique_ptr<PlanarMapping2d> XZPlanarMapping(real scale_x, real scale_z, real ofs_x, real ofs_z) {
        using Geometry::Vector;
        return unique_ptr<PlanarMapping2d>(new PlanarMapping2d (Vector{1,0,0}*scale_x,
                                                                Vector{0,0,1}*scale_z,
                                                                ofs_x, ofs_z));
    }

} } }

#endif // PLANARMAPPING2D_HH_20130811
