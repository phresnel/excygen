// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef AABB_HH_INCLUDED_20130718
#define AABB_HH_INCLUDED_20130718

#include "Geometry/Point.hh"
#include "Geometry/Ray.hh"
#include <algorithm>
#include <stdexcept>
#include <tuple>
#include "optional.hh"

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

        inline constexpr int longest_axis(real width, real height, real depth) noexcept {
            return (width>height) ? ((width>depth) ? 0 : 2) :
                   (height>depth) ? 1 : 2;
        }
    }



    struct AABB
    {
        constexpr Geometry::Point const& min() const noexcept { return min_; }
        constexpr Geometry::Point const& max() const noexcept { return max_; }

        constexpr AABB() = delete;
        AABB (AABB const &) = default;
        AABB& operator= (AABB const &) = default;

        constexpr AABB (Geometry::Point const &min, Geometry::Point const &max)
            : min_(detail::enforce_min_lt_max(min, max)), max_(max)
        {
        }

        constexpr AABB (Geometry::Point const &m)
            : min_(m), max_(m)
        {
        }

    private:
        Geometry::Point min_, max_;
    };

    inline std::ostream& operator<< (std::ostream &os, AABB const &aabb) {
        return os << "aabb{" << aabb.min() << "," << aabb.max() << "}";
    }

    inline AABB union_(AABB const &b, Geometry::Point const &p) noexcept {
        return {Geometry::Point(min(b.min().x, p.x), min(b.min().y, p.y), min(b.min().z, p.z)),
                Geometry::Point(max(b.max().x, p.x), max(b.max().y, p.y), max(b.max().z, p.z)) };
    }

    inline constexpr real left  (AABB const &aabb) noexcept { return aabb.min().x; }
    inline constexpr real right (AABB const &aabb) noexcept { return aabb.max().x; }
    inline constexpr real bottom(AABB const &aabb) noexcept { return aabb.min().y; }
    inline constexpr real top   (AABB const &aabb) noexcept { return aabb.max().y; }
    inline constexpr real front (AABB const &aabb) noexcept { return aabb.min().z; }
    inline constexpr real back  (AABB const &aabb) noexcept { return aabb.max().z; }

    inline constexpr real width (AABB const &aabb) noexcept { return right(aabb)-left(aabb); }
    inline constexpr real height(AABB const &aabb) noexcept { return top(aabb)-bottom(aabb); }
    inline constexpr real depth (AABB const &aabb) noexcept { return back(aabb)-front(aabb); }
    inline constexpr real extent(AABB const &aabb, int i) noexcept {
        return i==0?width(aabb):
               i==1?height(aabb):
               i==2?depth(aabb):
               throw std::logic_error("extent(AABB,i): i must be 0, 1 or 2");
    }

    inline constexpr Geometry::Point center(AABB const &aabb) noexcept {
        using namespace Geometry;
        return static_cast<Point>(static_cast<Vector>(aabb.min())*0.5 +
                                  static_cast<Vector>(aabb.max())*0.5);
    }

    inline constexpr real center(AABB const &aabb, int axis) noexcept {
        return aabb.min()[axis]*0.5 + aabb.max()[axis]*0.5;
    }

    inline constexpr int longest_axis(AABB const &aabb) noexcept {
        return detail::longest_axis(width(aabb), height(aabb), depth(aabb));
    }

    static_assert(longest_axis({{0,0,0},{3,2,1}}) == 0, "longest_axis(AABB) failure");
    static_assert(longest_axis({{0,0,0},{1,3,2}}) == 1, "longest_axis(AABB) failure");
    static_assert(longest_axis({{0,0,0},{1,2,3}}) == 2, "longest_axis(AABB) failure");


    inline std::tuple<AABB, AABB> split(AABB const &bb, int axis) noexcept {
        const real c = center(bb, axis);

        if (axis==0)
            return std::make_tuple(AABB(bb.min(), {c, bb.max().y, bb.max().z}),
                                   AABB(          {c, bb.min().y, bb.min().z}, bb.max()));
        if (axis==1)
            return std::make_tuple(AABB(bb.min(), {bb.max().x, c, bb.max().z}),
                                   AABB(          {bb.min().x, c, bb.min().z}, bb.max()));
        if (axis==2)
            return std::make_tuple(AABB(bb.min(), {bb.max().x, bb.max().y, c}),
                                   AABB(          {bb.min().x, bb.min().y, c}, bb.max()));
        throw std::logic_error("split(AABB,int) -> axis!=[0,1,2]");
    }


    inline optional<tuple<real,real>> intersect(AABB const &box, Geometry::Ray const &ray) noexcept {
        real t0 = -real_max;
        real t1 = real_max;

        // X
        {
                real i = real(1) / ray.direction.x(),
                     near = (box.min().x - ray.origin.x) * i,
                     far  = (box.max().x - ray.origin.x) * i;
                if (near > far) swap (near, far);
                t0 = near > t0 ? near : t0;
                t1 = far < t1 ? far : t1;
                if (t0 > t1) return optional<tuple<real,real>>();
        }

        // Y
        {
                real i = real(1) / ray.direction.y(),
                     near = (box.min().y - ray.origin.y) * i,
                     far  = (box.max().y - ray.origin.y) * i;
                if (near > far) swap (near, far);
                t0 = near > t0 ? near : t0;
                t1 = far < t1 ? far : t1;
                if (t0 > t1) return optional<tuple<real,real>>();
        }

        // Z
        {
                real i = real(1) / ray.direction.z(),
                     near = (box.min().z - ray.origin.z) * i,
                     far  = (box.max().z - ray.origin.z) * i;
                if (near > far) swap (near, far);
                t0 = near > t0 ? near : t0;
                t1 = far < t1 ? far : t1;
                if (t0 > t1) return optional<tuple<real,real>>();
        }
        return make_tuple (t0, t1);
    }
}

#endif // AABB_HH_INCLUDED_20130718

