// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef REAL_HH_INCLUDED_20130708
#define REAL_HH_INCLUDED_20130708

#include <tuple>
#include <algorithm>
#include <random>

namespace excyrender {
    typedef double real;
    static constexpr real epsilon = real(0.00001);
    static constexpr real pi = real(3.14159265358979323846);

    using std::get;
    using std::tuple;
    using std::min;
    using std::max;
    using std::minmax;

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

    constexpr inline
    real fabs(real n) noexcept {
        return n<0 ? -n : n;
    }

    using std::mt19937;
    typedef std::uniform_real_distribution<real> uniform_real_distribution;
}

#endif // REAL_HH_INCLUDED_20130708

