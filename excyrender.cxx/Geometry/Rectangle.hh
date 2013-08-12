// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef RECTANGLE_HH_INCLUDED_20130812
#define RECTANGLE_HH_INCLUDED_20130812

#include "Point2d.hh"

namespace excyrender { namespace Geometry {

    class Rectangle final {
        template <typename T>
        static constexpr T enforce_precondition(bool cond, T const & val) {
            return cond ? val : throw std::logic_error("Rectangle precondition error. Requirements: "
                                                       "left<=right && bottom<=top");

        }

    public:

        Rectangle() = delete;

        constexpr Rectangle(Point2d const &min, Point2d const &max)
        : min_(enforce_precondition(min.x<=max.x, min))
        , max_(enforce_precondition(min.y<=max.y, max))
        {
        }

        constexpr Point2d operator() (real u, real v) noexcept {
            return { (1-u)*min_.x + u*max_.x,
                     (1-v)*min_.y + v*max_.y };
        }

        constexpr real left()   noexcept { return min_.x; }
        constexpr real right()  noexcept { return max_.x; }
        constexpr real top()    noexcept { return min_.y; }
        constexpr real bottom() noexcept { return max_.y; }

        constexpr real width()  noexcept { return right() - left(); }
        constexpr real height() noexcept { return bottom() - top(); }

    private:
        Point2d min_, max_;
    };

} }

#endif // RECTANGLE_HH_INCLUDED_20130812
