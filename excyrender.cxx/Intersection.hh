// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef INTERSECTION_HH_INCLUDED_20130718
#define INTERSECTION_HH_INCLUDED_20130718

#include "DifferentialGeometry.hh"
#include "Photometry/BSDF/BSDF.hh"

namespace excyrender {

    struct Intersection
    {
        DifferentialGeometry dg;
        Photometry::BSDF::BSDF bsdf;
    };
    
    inline real distance(Intersection const &i) noexcept {
        return i.dg.d;
    }
}

#endif // INTERSECTION_HH_INCLUDED_20130718

