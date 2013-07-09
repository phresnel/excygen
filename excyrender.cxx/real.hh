// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef REAL_HH_INCLUDED_20130708
#define REAL_HH_INCLUDED_20130708

namespace excyrender {
    typedef float real;

    template <typename T> constexpr inline
    T saturate(T v, T min, T max) {
        return v<min?min : v>max?max : v;
    }
}

#endif // REAL_HH_INCLUDED_20130708

