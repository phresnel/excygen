// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "raytrace.hh"
#include "Photometry/Lighting.hh"

#include "Photometry/Texture/ImageTexture.hh"
#include "Photometry/Texture/UVMapping2d.hh"

#include "Primitives/PrimitiveList.hh"
#include "SurfaceIntegrators/Path.hh"
#include "ImageFormat/PPM.hh"
#include "Geometry/Direction.hh"

#include <Python.h>
#include <boost/python.hpp>

#include <iostream>

// Enable std::shared_ptr for use in our API.
namespace std {
    template<class T> const T* get_pointer(const std::shared_ptr<T>& p)
    {
        return p.get();
    }

    template<class T> T* get_pointer(std::shared_ptr<T>& p)
    {
        return p.get();
    }
}


namespace PyAPI {
    std::string api_version()
    {        
        return "<master>";
    }

    std::string api_compile_date()
    {
        return __DATE__;
    }


    struct Renderer {
        int width, height, samples_per_pixel;

        Renderer(int width, int height, int samples_per_pixel) :
            width(width), height(height), samples_per_pixel(samples_per_pixel) {}
    };

    struct SurfaceIntegrator {
        int maximum_recursion = 5;

        SurfaceIntegrator() {}
    };

    struct SunSky {
        excyrender::Geometry::Direction direction;
        excyrender::Photometry::RGB sunlight;
        excyrender::Photometry::RGB skylight;

        SunSky(excyrender::Geometry::Direction const &direction,
               excyrender::Photometry::RGB const &sunlight,
               excyrender::Photometry::RGB const &skylight)
            : direction(direction),
              sunlight(sunlight),
              skylight(skylight)
        {}
    };


    void render(Renderer const &renderer,
                SunSky const &sunSky)
    {
        using namespace excyrender;

        SurfaceIntegrator surface_integrator;

        std::clog << "rendering (" << renderer.width << "x" << renderer.height << "@"
                  << renderer.samples_per_pixel << "spp)" << std::endl;

        std::vector<Photometry::RGB> pixels(renderer.width*renderer.height);
        std::vector<DebugPixel> debug(renderer.width*renderer.height);

        std::vector<std::shared_ptr<Photometry::LightSource>> const lightSources({
            std::shared_ptr<Photometry::LightSource>(new Photometry::Directional (
                  sunSky.direction,
                  Photometry::Spectrum::FromRGB(400,800,8,sunSky.sunlight))
            )
        });

        Primitives::PrimitiveList const primitive({});

        auto const integrator =
                SurfaceIntegrators::Path(
                    surface_integrator.maximum_recursion,
                    primitive,
                    lightSources,
                    [&sunSky](Geometry::Direction const &) {
                        return Photometry::Spectrum::FromRGB(400,800,8,sunSky.skylight);
                    }
                );

        raytrace (renderer.width, renderer.height, renderer.samples_per_pixel,
                  integrator, pixels, debug);

        ImageFormat::ppm (std::cout,
                          renderer.width, renderer.height, pixels);
    }
}



void photometry_api()
{
    using namespace boost::python;
    using namespace PyAPI;

    class_<excyrender::Photometry::RGB>
      ("RGB")
      .def(init<excyrender::real, excyrender::real, excyrender::real>())
    ;
}



void material_api()
{
    using namespace boost::python;
    using namespace PyAPI;

    using excyrender::real;
    using namespace excyrender::Photometry::Texture;
    using namespace excyrender::Photometry;

    class_<ImageTexture<Spectrum>, std::shared_ptr<ImageTexture<Spectrum>>, boost::noncopyable>
      ("ImageTexture", init<std::shared_ptr<Mapping2d>, std::string>())
    ;

    class_<UVMapping2d, std::shared_ptr<UVMapping2d>>
      ("UVMapping2d", init<real,real,real,real>());


    implicitly_convertible<std::shared_ptr<UVMapping2d>,
                           std::shared_ptr<Mapping2d> >();
}



void geometry_api()
{
    using namespace boost::python;
    using namespace PyAPI;

    // Direction
    excyrender::Geometry::Direction (*direction)(excyrender::real,excyrender::real,excyrender::real) = &excyrender::Geometry::direction;
    def("direction", direction);

    class_<excyrender::Geometry::Direction>
      ("Direction", init<excyrender::Geometry::Direction>())
      .def("x", &excyrender::Geometry::Direction::x)
      .def("y", &excyrender::Geometry::Direction::y)
      .def("z", &excyrender::Geometry::Direction::z);
}



void rendering_api()
{
    using namespace boost::python;
    using namespace PyAPI;

    def("render", render, (arg("renderer"), arg("sunsky")));

    class_<Renderer>("Renderer", init<int, int, int>((arg("width")=640, arg("height")=480, arg("sample_per_pixel")=16)))
      .def_readonly("width", &Renderer::width)
      .def_readonly("height", &Renderer::height)
      .def_readonly("samples_per_pixel", &Renderer::samples_per_pixel);
}



void skylight_api()
{
    using namespace boost::python;
    using namespace PyAPI;

    class_<SunSky>("SunSky", init<excyrender::Geometry::Direction,
                                  excyrender::Photometry::RGB,
                                  excyrender::Photometry::RGB>((arg("direction"),
                                                                arg("sunlight")=excyrender::Photometry::RGB(10,10,10),
                                                                arg("skylight")=excyrender::Photometry::RGB(1,2,3))))
      .def_readonly("direction", &SunSky::direction);
}



BOOST_PYTHON_MODULE(excygen) {
    using namespace boost::python;
    using namespace PyAPI;

    //== API Meta ==================================================================================
    def("api_version", api_version);
    def("api_compile_date", api_compile_date);            
    geometry_api();
    photometry_api();
    material_api();
    rendering_api();
    skylight_api();
}



void PythonAPI(std::string const &script)
{
    std::clog << "excygen python api" << std::endl;
    //Py_SetProgramName(argv[0]);

    std::clog << "initialize python environment..." << std::endl;
    Py_Initialize();

    std::clog << "importing excygen API into python environment..." << std::endl;
    initexcygen(); // This function is generated by BOOST_PYTHON_MODULE and
                   // enriches the Python runtime environment with our API.

    try {
        using namespace boost::python;
        object main_module = import("__main__");
        object main_namespace = main_module.attr("__dict__");

        std::clog << "-- running script ----------------------------------------" << std::endl;
        exec(script.c_str(),
             main_namespace);
        std::clog << "----------------------------------------------------------" << std::endl;
    } catch (boost::python::error_already_set const &) {
        PyErr_Print();
    }

    std::clog << "finalizing..." << std::endl;
    Py_Finalize();

    std::clog << "hope you are happy!" << std::endl;
}
