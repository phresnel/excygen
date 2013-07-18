// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef AABB_HH_INCLUDED_20130718
#define AABB_HH_INCLUDED_20130718

#include <Geometry/Point.hh>
#include <algorithm>
#include <stdexcept>

namespace excyrender {
    namespace detail {
        inline constexpr Geometry::Point enforce_min_lt_max(
            Geometry::Point const &min, Geometry::Point const &max)
        {
            return (min.x<=max.x &&
                    min.y<=max.y &&
                    min.z<=max.z) ?
                   min : 
                   throw std::logic_error("'AABB(min,max)': Only valid for 'min<max'");
        }
    }



    struct AABB
    {
        const Geometry::Point min, max;
        
        constexpr AABB() = delete;
        
        constexpr AABB (Geometry::Point const &min, Geometry::Point const &max)
            : min(detail::enforce_min_lt_max(min, max)), max(max)
        {
        }
    };



    inline constexpr real left  (AABB const &aabb) noexcept { return aabb.min.x; }
    inline constexpr real right (AABB const &aabb) noexcept { return aabb.max.x; }
    inline constexpr real bottom(AABB const &aabb) noexcept { return aabb.min.y; }
    inline constexpr real top   (AABB const &aabb) noexcept { return aabb.max.y; }
    inline constexpr real front (AABB const &aabb) noexcept { return aabb.min.z; }
    inline constexpr real back  (AABB const &aabb) noexcept { return aabb.max.z; }

    inline constexpr real width (AABB const &aabb) noexcept { return right(aabb)-left(aabb); }
    inline constexpr real height(AABB const &aabb) noexcept { return top(aabb)-bottom(aabb); }
    inline constexpr real depth (AABB const &aabb) noexcept { return back(aabb)-front(aabb); }
    inline constexpr real extent(AABB const &aabb, int i) noexcept {
        return i==0?width(aabb):
               i==1?height(aabb):
               i==2?depth(aabb):
               throw std::logic_error("extent(AABB,i): i must be 0, 1 or 2");
    }
}

#endif // AABB_HH_INCLUDED_20130718

