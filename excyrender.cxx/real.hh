#ifndef REAL_HH_INCLUDED_20130708
#define REAL_HH_INCLUDED_20130708

namespace excygen {
    typedef float real;

    template <typename T> constexpr inline
    T saturate(T v, T min, T max) {
        return v<min?min : v>max?max : v;
    }
}

#endif // REAL_HH_INCLUDED_20130708

