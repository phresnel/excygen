// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef MATERIAL_HH_INCLUDED_20130809
#define MATERIAL_HH_INCLUDED_20130809

#include "Photometry/BSDF/BSDF.hh"

namespace excyrender { namespace Photometry { namespace Material {
    struct Material {
        virtual Surface::BSDF bsdf(DifferentialGeometry const &) const noexcept = 0;
    };
} } }


#endif // MATERIAL_HH_INCLUDED_20130809

