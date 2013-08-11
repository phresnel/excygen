// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef MAPPING2D_HH_INCLUDED_20130810
#define MAPPING2D_HH_INCLUDED_20130810

#include "DifferentialGeometry.hh"
#include "TexCoords2d.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    class Mapping2d {
    public:
        virtual ~Mapping2d() {}
        virtual TexCoords2d operator() (DifferentialGeometry const &) const noexcept = 0;
    };

} } }

#endif // MAPPING2D_HH_INCLUDED_20130810
