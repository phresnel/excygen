// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef DEBUGPIXEL_HH_INCLUDED_20130802
#define DEBUGPIXEL_HH_INCLUDED_20130802

#include <cstdint>

namespace excyrender {

    struct DebugPixel {
        uint8_t traversal0 = 0;
        void operator += (DebugPixel const &rhs) noexcept {
            traversal0 += rhs.traversal0;
        }
    };

    extern DebugPixel *current_debug;
    #pragma omp threadprivate(current_debug)
}

#endif // DEBUGPIXEL_HH_INCLUDED_20130802
