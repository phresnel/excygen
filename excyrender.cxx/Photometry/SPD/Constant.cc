// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Constant.hh"
#include "Photometry/SPD/Regular.hh"
#include "Photometry/CIEMatchingCurves.hh"
#include <array>

namespace excyrender { namespace Photometry { namespace SPD {

    std::tuple<real,real,real> Constant::toXYZ() const
    {
        using namespace CIEMatchingCurves;
        return Regular(cie_start, cie_end, std::array<real,1>{{c_}}).toXYZ();
    }

} } }
