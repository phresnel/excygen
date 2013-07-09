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

