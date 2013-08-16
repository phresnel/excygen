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
#include "Shapes/Triangle.hh"
#include "Shapes/Terrain2d.hh"
#include "Primitives/PrimitiveList.hh"
#include "Primitives/PrimitiveFromShape.hh"
#include "Primitives/PrimitiveFromFiniteShape.hh"

#include "Photometry/Lighting.hh"
#include "Photometry/Material/BSDFPassthrough.hh"
#include "Photometry/Material/Lambertian.hh"

#include "Photometry/Texture/PlanarMapping2d.hh"
#include "Photometry/Texture/ConstantTexture.hh"
#include "Photometry/Texture/MulTexture.hh"
#include "Photometry/Texture/AddTexture.hh"
#include "Photometry/Texture/LerpTexture.hh"
#include "Photometry/Texture/ImageTexture.hh"

#include "Primitives/BoundingIntervalHierarchy.hh"
#include "DebugPixel.hh"

#include "Scripting/Et1.hh"

#include <iostream>
#include <functional>
#include <vector>
#include <memory>
#include <ctime>

namespace excyrender {

    class RNG {
    public:
        RNG(uint_fast32_t seed)
            : data(new data_t{mt19937(seed),
                              uniform_real_distribution(0,real(1))})
        {
        }

        real operator() () {
            return data->d(data->mt);
        }

    private:
        struct data_t {
            mt19937 mt;
            uniform_real_distribution d;
        };
        std::shared_ptr<data_t> data;
    };

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
                    const auto ray = Ray{Point{0,0.5,0}, Geometry::direction(u-0.5, v-0.5, 0.8)};
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


#include "Primitives/BoundingIntervalHierarchy.hh"

int main () {
    try {
        using namespace excyrender;
        using namespace Primitives;
        using namespace Photometry;
        using namespace Geometry;
        using Surface::BxDF;
        using Surface::BSDF;
        using namespace Photometry::Texture;

        const auto width = 800,
                   height = 800;
        const auto samples_per_pixel = 1;
        std::vector<Photometry::RGB> pixels(width*height);
        std::vector<DebugPixel> debug(width*height);


        Primitives::BoundingIntervalHierarchyBuilder builder;
        /*
        builder.add({std::shared_ptr<Primitives::FinitePrimitive>(new
                         PrimitiveFromFiniteShape (std::shared_ptr<Shapes::FiniteShape>(new Shapes::Sphere ({-1.0,0.0,5}, 1)),
                         std::shared_ptr<const Material::Material>(new Material::BSDFPassthrough(BSDF({std::shared_ptr<BxDF>(new Surface::Lambertian (Spectrum::FromRGB(400,800,8, {1,0.3,0.3})))})))
                     )),
                     std::shared_ptr<Primitives::FinitePrimitive>(new
                         PrimitiveFromFiniteShape (std::shared_ptr<Shapes::FiniteShape>(new Shapes::Sphere ({1.0,0.0,5}, 1)),
                         std::shared_ptr<const Material::Material>(new Material::BSDFPassthrough(BSDF({std::shared_ptr<BxDF>(new Surface::Lambertian (Spectrum::Gray(400,800,8, 1)))})))
                     )),
                     std::shared_ptr<Primitives::FinitePrimitive>(new
                         PrimitiveFromFiniteShape (std::shared_ptr<Shapes::FiniteShape>(new Shapes::Triangle({0,0,5},{-1,1,5},{1,1,5})),
                         std::shared_ptr<const Material::Material>(new Material::BSDFPassthrough(BSDF ({ std::shared_ptr<BxDF>( new Surface::Lambertian (Spectrum::FromRGB(400,800,8,{0.6,1.0,0.4})) ) })))
                     )),
                     std::shared_ptr<Primitives::FinitePrimitive>(new
                         PrimitiveFromFiniteShape (std::shared_ptr<Shapes::FiniteShape>(new Shapes::Triangle({0,0,5},{-1,-1,5},{1,-1,5})),
                         std::shared_ptr<const Material::Material>(new Material::BSDFPassthrough(BSDF ({ std::shared_ptr<BxDF>( new Surface::Lambertian (Spectrum::FromRGB(400,800,8,{0.6,1.0,0.4})) ) })))
                     ))
                    });

        for (int i=0; i<10000; ++i) {
            float x = rand() / (float)RAND_MAX * 100 - 50;
            float z = 4+rand() / (float)RAND_MAX * 100 - 10;
            float r = 0.05 + rand() / (float)RAND_MAX * 0.3;
            float y = r-1;//rand() / (float)RAND_MAX * 10;

            float r_ = rand() / (float)RAND_MAX * 0.6 + 0.4;
            float g_ = rand() / (float)RAND_MAX * 0.6 + 0.4;
            float b_ = rand() / (float)RAND_MAX * 0.6 + 0.4;

            builder.add(std::shared_ptr<Primitives::FinitePrimitive>(new
                         PrimitiveFromFiniteShape (std::shared_ptr<Shapes::FiniteShape>(new Shapes::Sphere ({x,y,z}, r)),
                         std::shared_ptr<const Material::Material>(new Material::BSDFPassthrough(BSDF({
                             std::shared_ptr<BxDF>(new Surface::Lambertian (Spectrum::FromRGB(400,800,8, {r_,g_,b_})))
                         })))
                       )));
        }
        */

        builder.add(std::shared_ptr<Primitives::FinitePrimitive>(new
                             PrimitiveFromFiniteShape (std::shared_ptr<Shapes::FiniteShape>(
                                                          new Shapes::Terrain2d(
                                                               Geometry::Rectangle({-100,-100},{100,100}),
                                                               Geometry::Rectangle({0,0},{100,100}),
                                                               512,
                                                               //[](real u,real v) { return -4 + 5*sin(u) * sin(v); }
                                                               Nature::Et1::compile("sqrt(1,2,3,foo())+-4+sin(x)+sin(y)+(3*2)")
                                                           )),
                             std::shared_ptr<Material::Material>(new Material::Lambertian(
                                  shared_ptr<SpectrumTexture>(new ColorImageTexture(Photometry::Texture::XZPlanarMapping(0.4,0.4,0,0),
                                                                                    "Rock_07_UV_H_CM_1.jpg"))
                             ))
                         ))
                );


        PrimitiveList const primitive({
                         builder.finalize(20)
                         /*std::shared_ptr<Primitive>(new
                             PrimitiveFromShape (std::shared_ptr<Shapes::Shape>(new Shapes::Plane(Shapes::Plane::FromPointNormal({0,-1,0},normal(0,1,0)))),
                             //std::shared_ptr<const Material::Material>(new Material::BSDFPassthrough(BSDF ({ std::shared_ptr<BxDF>( new Surface::Lambertian (Spectrum::Gray(400,800,8,real(1))) ) })))
                             std::shared_ptr<Material::Material>(new Material::Lambertian(
                                  shared_ptr<SpectrumTexture>(new ColorImageTexture(Photometry::Texture::XZPlanarMapping(0.4,0.4,0,0),
                                                                                    "loose_gravel_9261459 (mayang.com).JPG"))
                             ))
                         ))*/
                        });

        std::vector<std::shared_ptr<LightSource>> const lightSources({
            std::shared_ptr<LightSource>(new Directional (direction(1,0.5,-1), Spectrum::FromRGB(400,800,8,{8,7,7})))
        });
        auto const integrator = SurfaceIntegrators::Path(5, primitive, lightSources,
                                                         [](Geometry::Direction const &) {
                                                            return Spectrum::FromRGB(400,800,8,{1,2,3});
                                                         }
                                                        );

        raytrace (width, height, samples_per_pixel, integrator, pixels, debug);
        if (0) {
            for (int y=0; y!=height; ++y) {
                for (int x=y%2; x<width; x+=2) {
                    pixels[y*width+x] = Photometry::RGB(0.5,0.5,1.0) * (debug[y*width+x].traversal0) * 0.025;
                }
            }
        }
        ImageFormat::ppm (std::cout, width, height, pixels);
    } catch (std::exception &e) {
        std::cerr << "error:" << e.what() << "(" << typeid(e).name() << ")\n";
    }
}

