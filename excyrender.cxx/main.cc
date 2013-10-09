// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Photometry/RGB.hh"
#include "ImageFormat/PPM.hh"
#include "Geometry/Ray.hh"
#include "Geometry/Direction.hh"
#include "Photometry/CIEMatchingCurves.hh"
#include "Photometry/ColorSpace.hh"
#include "Geometry/Transform.hh"

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
#include "Primitives/factories.hh"
#include "DebugPixel.hh"

#include "Scripting/Et1.hh"
#include "Scripting/Python.hh"
#include <Python.h>

#include <boost/algorithm/string.hpp>

#include <iostream>
#include <fstream>
#include <functional>
#include <vector>
#include <memory>
#include <ctime>

#include "Primitives/BoundingIntervalHierarchy.hh"
#include "main_test.hh"
#include "raytrace.hh"

int main (int argc, char *argv[]) {

    if (argc >= 2 && argv[1]==std::string("test")) {
        // Short circuit the CLI arguments.
        argc -= 2;
        argv += 2;
        std::vector<std::string> args;
        args.push_back("excygen test");
        while (*argv != 0) {
            args.push_back(*argv);
            ++argv;
        }
        std::vector<char*> args_c_str;
        for (auto &s : args)
            args_c_str.emplace_back(const_cast<char*>(s.c_str()));
        return unit_tests(args_c_str.size(), &args_c_str[0]);
    } else if (argc == 2 && boost::algorithm::ends_with(std::string(argv[1]), ".py")) {
        void PythonAPI(std::string const &);

        std::ifstream t(argv[1]);
        if (!t.good()) {
            std::cerr << "could not open \"" << argv[1] << "\"." << std::endl;
            return 1;
        }
        std::stringstream buffer;
        buffer << t.rdbuf();

        PythonAPI(buffer.str());

        return 0;
    } else if (argc != 1) {
        std::cerr << "invalid command line. use:\n"
                  << " * <excygen>\n"
                  << " * <excygen> test     <-- this will execute all unit tests\n"
                  << " * <excygen> test -?  <-- this shows available options for unit tests\n"
                  << " * <excygen> <python-file>  <-- runs the Python API over <python-file>\n";
        return 1;
    }

    Py_SetProgramName(argv[0]);
    Py_Initialize();

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
        const auto samples_per_pixel = 200;
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

        builder.add(create_terrain2d_alpha(Geometry::Rectangle({-1000,-1000},{1000,1000}),
                                           2048,
                                           Scripting::Python::PyHeightFun("test"),
                                           std::shared_ptr<Photometry::Material::Material>(
                                              new Photometry::Material::Lambertian(Photometry::Texture::constantTexture(Spectrum::FromRGB(400,800,8, {0.7,0.7,0.7}))))
                                         ));


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

    Py_Finalize();
}

