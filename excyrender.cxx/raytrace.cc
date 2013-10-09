// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "raytrace.hh"
#include "RNG.hh"

#include "Geometry/Transform.hh"
#include "Photometry/ColorSpace.hh"

namespace excyrender {

    void raytrace (int width, int height, int samples_per_pixel,
                   std::function<Photometry::Spectrum(Geometry::Ray const &, std::function<float()>)> integrate,
                   std::vector<Photometry::RGB> &pixels,
                   std::vector<DebugPixel> &debug)
    {
        using namespace Geometry;
        using namespace Photometry;

        auto tick_log = clock();

        for (auto y=0; y!=height; ++y) {
            #pragma omp parallel for
            for (auto x=0; x<width; ++x) {
                /*DebugPixel *cd =*/
                current_debug = &debug[y*width+x];

                RNG rng(7*y*width+3*x);

                Spectrum sum = Spectrum::Black(400,800,8);
                for (auto i=0; i!=samples_per_pixel; ++i) {
                    const auto u = (x + rng()-real(0.5)) / real(width),
                               v = 1 - (y + rng()-real(0.5)) / real(height);
                    const auto ray = //inverse(Transform::LookAt({0,0,0}, {0,0,1},{0,1,0})) *
                                     Ray{Point{0,0,0}, Geometry::direction(u-0.5, v-0.5, 0.8)};
                    sum += integrate(ray, rng) * (real(1) / samples_per_pixel);
                    current_debug = 0;
                }

                const auto XYZ = sum.toXYZ();
                const auto RGB = Photometry::ColorSpace::XYZ_to_sRGB(XYZ);
                pixels[y*width+x] = Photometry::RGB(get<0>(RGB), get<1>(RGB), get<2>(RGB));

                /*#pragma omp critical
                std::cout << (int)cd->traversal0 << ":";*/
            }

            /*#pragma omp critical
            std::cout << "\n";*/

            auto curr = clock();
            if (curr - tick_log > CLOCKS_PER_SEC) {
                tick_log = curr;
                std::clog << y << '/' << height << std::endl;
            }
        }
    }
}
