// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef DIFFERENTIALGEOMETRY_HH_INCLUDED_20130712
#define DIFFERENTIALGEOMETRY_HH_INCLUDED_20130712

#include "Geometry/Point.hh"
#include "Geometry/Normal.hh"
#include "Geometry/Direction.hh"
#include "Geometry/Vector.hh"

namespace excyrender {
    struct DifferentialGeometry {
        real d;
        Geometry::Point poi;
        Geometry::Normal nn;
        real u, v;
        Geometry::Vector dpdu;
        // -- shape :: Shape <-- This is as PBRT has it, but it would produce a 
        //--                    recursive dependency
    };
    
    Geometry::Direction worldToLocal (DifferentialGeometry const &dg, Geometry::Direction const &dir) {
        const Geometry::Vector n {dg.nn.x, dg.nn.y, dg.nn.z},
                               s = normalize(dg.dpdu),
                               t = cross(n, s),
                               v {dir.x, dir.y, dir.z};
        return {dot(v, s), dot(v,n), dot(v,t)};
    }
    
    Geometry::Direction localToWorld (DifferentialGeometry const &dg, Geometry::Direction const &dir) {
        const Geometry::Vector n {dg.nn.x, dg.nn.y, dg.nn.z},
                               s = normalize(dg.dpdu),
                               t = cross(n, s);
        return {s.x*dir.x + n.x*dir.y + t.x*dir.z,
                s.y*dir.x + n.y*dir.y + t.y*dir.z,
                s.z*dir.x + n.z*dir.y + t.z*dir.z};
    }
}


#endif // DIFFERENTIALGEOMETRY_HH_INCLUDED_20130712

