// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef UVMAPPING2D_HH_INCLUDED_20130811
#define UVMAPPING2D_HH_INCLUDED_20130811

#include "Mapping2d.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    struct UVMapping2d final : Mapping2d {

        UVMapping2d(real scale_u, real scale_v, real offset_u, real offset_v)
        : scale_u(scale_u), scale_v(scale_v), offset_u(offset_u), offset_v(offset_v)
        {}

        TexCoords2d operator() (DifferentialGeometry const &dg) const noexcept {
            return {scale_u * dg.u + offset_u,
                    scale_v * dg.v + offset_v};
        }

    private:
        real scale_u, scale_v;
        real offset_u, offset_v;
    };

} } }

#endif // UVMAPPING2D_HH_INCLUDED_20130811
