// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Photometry/RGB.hh"
#include "ImageFormat/PPM.hh"
#include "Geometry/Ray.hh"
#include "Geometry/Direction.hh"
#include "Photometry/CIEMatchingCurves.hh"
#include "Photometry/ColorSpace.hh"

#include "SurfaceIntegrators/Path.hh"

#include "Shapes/Sphere.hh"
#include "Shapes/Plane.hh"
#include "Primitives/PrimitiveList.hh"
#include "Primitives/PrimitiveFromShape.hh"

#include "Photometry/Lighting.hh"

#include <iostream>
#include <functional>
#include <vector>
#include <memory>

namespace excyrender {
    void raytrace (int width, int height,
                   std::function<Photometry::Spectrum(Geometry::Ray const &, std::function<float()>)> integrate,
                   std::vector<Photometry::RGB> &pixels)
    {
        using namespace Geometry;
        using namespace Photometry;

        auto rng = [] { return rand() / static_cast<real>(RAND_MAX); };

        for (auto y=0; y!=height; ++y) {
            for (auto x=0; x!=width; ++x) {
                const auto u = x / real(width),
                           v = 1 - y / real(height);

                Spectrum sum = Spectrum::Black(400,800,8);
                const auto num_samples = 2;
                for (auto i=0; i!=num_samples; ++i) {
                    const auto ray = Ray{Point{0,0,0}, Geometry::direction(u-0.5, v-0.5, 1)};                
                    sum += integrate(ray, rng) * (real(1) / num_samples);
                }                

                const auto XYZ = sum.toXYZ();
                const auto RGB = Photometry::ColorSpace::XYZ_to_sRGB(XYZ);
                pixels[y*width+x] = Photometry::RGB(get<0>(RGB), get<1>(RGB), get<2>(RGB));
            }
        }
    }
}


int main () {
    using namespace excyrender;
    using namespace Primitives;
    using namespace Photometry;
    using namespace Surface;
    using namespace Geometry;

    const auto width = 256,
               height = 256;
    std::vector<Photometry::RGB> pixels(width*height);

    /*auto const integrator = [] (Geometry::Ray const &r) {
        return Photometry::RGB{r.direction.x+real(0.5), r.direction.y+real(0.5), 0};
    };*/

    PrimitiveList const primitive({
                     std::shared_ptr<Primitive>(new
                         PrimitiveFromShape (std::shared_ptr<Shapes::Shape>(new Shapes::Sphere ({-1.0,0.0,5}, 1)),
                                             BSDF({std::shared_ptr<BxDF>(new Lambertian (Spectrum::FromRGB(400,800,8, {1,0.3,0.3})))
                                                 })
                                        )),
                     std::shared_ptr<Primitive>(new
                         PrimitiveFromShape (std::shared_ptr<Shapes::Shape>(new Shapes::Sphere ({1.0,0.0,5}, 1)),
                                             BSDF({std::shared_ptr<BxDF>(new Lambertian (Spectrum::FromRGB(400,800,8, {1,1,1})))
                                                 })
                                        )),
                     std::shared_ptr<Primitive>(new
                         PrimitiveFromShape (std::shared_ptr<Shapes::Shape>(new Shapes::Plane(Shapes::Plane::FromPointNormal({0,-1,0},normal(0,1,0)))),
                                             BSDF ({ std::shared_ptr<BxDF>( new Lambertian (Spectrum::Gray(400,800,8,1)) ) })
                                                  ))
                    });
    std::vector<std::shared_ptr<LightSource>> const lightSources({
        std::shared_ptr<LightSource>(new Directional (direction(0,1,0), Spectrum::FromRGB(400,800,8,{5,5,5})))
    });
    auto const integrator = SurfaceIntegrators::Path(8, primitive, lightSources);

    raytrace (width, height, integrator, pixels);
    ImageFormat::ppm (std::cout, width, height, pixels);
}

