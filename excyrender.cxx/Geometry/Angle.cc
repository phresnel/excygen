// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "Angle.hh"


//- Tests ------------------------------------------------------------------------------------------
#include "catch.hpp"
TEST_CASE( "Geometry/Angle", "Angle" ) {
    using namespace excyrender::Geometry;
    using namespace excyrender;
    REQUIRE((real)Radians(Angle(Degrees(90))) == Approx((real)Radians(Angle(Radians(1.57079633)))));
    REQUIRE((real)Radians(Angle(Degrees(89))) != Approx((real)Radians(Angle(Radians(1.57079633)))));

    REQUIRE((real)Radians(Angle(Degrees(45))) == Approx((real)Radians(Angle(Radians(0.5*1.57079633)))));
    REQUIRE((real)Radians(Angle(Degrees(44))) != Approx((real)Radians(Angle(Radians(0.5*1.57079633)))));
}
