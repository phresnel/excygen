// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Photometry/RGB.hh"
#include "ImageFormat/PPM.hh"
#include "Geometry/Ray.hh"

#include <iostream>
#include <functional>
#include <vector>

namespace excyrender {
    void raytrace (int width, int height,
                   std::function<Photometry::RGB(Geometry::Ray const &)> integrate,
                   std::vector<Photometry::RGB> &pixels)
    {
        using namespace Geometry;
        for (auto y=0; y!=height; ++y) {
            for (auto x=0; x!=width; ++x) {
                const auto u = x / real(width),
                           v = 1 - y / real(height);
                const auto ray = Ray{Point{0,0,0}, direction(u-0.5, v-0.5, 1)};
                pixels[y*width+x] = integrate(ray);
            }
        }
    }
}


int main () {
    using namespace excyrender;

    const auto width = 200,
               height = 200;
    std::vector<Photometry::RGB> pixels(width*height);

    auto const integrator = [] (Geometry::Ray const &r) {
        return Photometry::RGB{r.direction.x+real(0.5), r.direction.y+real(0.5), 0};
    };

    raytrace (width, height, integrator, pixels);

    ImageFormat::ppm (std::cout, width, height, pixels);
}

