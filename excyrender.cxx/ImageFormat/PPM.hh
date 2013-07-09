// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PPM_HH_INCLUDED_20130708
#define PPM_HH_INCLUDED_20130708

#include "real.hh"
#include <vector>
#include <iosfwd>

namespace excyrender {
    namespace Photometry {
        struct RGB;
    }

    namespace ImageFormat {
        void ppm (std::ostream &os, int width, int height, std::vector<Photometry::RGB> const &pixels);
    }
}

#endif // PPM_HH_INCLUDED_20130708

