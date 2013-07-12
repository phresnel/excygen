// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef REAL_HH_INCLUDED_20130708
#define REAL_HH_INCLUDED_20130708

#include <tuple>

namespace excyrender {
    typedef float real;

    using std::get;
    
    template <typename T=real>
      using triple = std::tuple<T,T,T>;
    
    template <typename T>
    triple<T> make_triple(T const &a, T const &b, T const &c) {
        return std::make_tuple(a,b,c);
    }

    template <typename T>
    constexpr inline
    T saturate(T v, T min, T max) noexcept {
        return v<min?min : v>max?max : v;
    }

    static const real pi = static_cast<real>(3.14159265358979323846);
    
    constexpr inline
    real fabs(real n) noexcept {
        return n<0 ? -n : n;
    }
}

#endif // REAL_HH_INCLUDED_20130708

