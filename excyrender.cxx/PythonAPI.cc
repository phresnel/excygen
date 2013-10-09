// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "raytrace.hh"
#include "Photometry/Lighting.hh"

#include "Photometry/Texture/ImageTexture.hh"
#include "Photometry/Texture/ConstantTexture.hh"
#include "Photometry/Texture/UVMapping2d.hh"
#include "Photometry/Texture/PlanarMapping2d.hh"
#include "Photometry/Material/Lambertian.hh"

#include "Primitives/PrimitiveList.hh"
#include "SurfaceIntegrators/Path.hh"
#include "ImageFormat/PPM.hh"

#include "Primitives/factories.hh"

#include "Geometry/Direction.hh"
#include "Geometry/Vector.hh"
#include "Geometry/Rectangle.hh"

#include "Scripting/Python.hh"
#include <Python.h>
#include <boost/python.hpp>
#include <boost/python/object.hpp>

#include <iostream>
#include <list>

// Enable std::shared_ptr for use in our API.
namespace std {
    template<class T> const T* get_pointer(const std::shared_ptr<T>& p) { return p.get(); }
    template<class T> T*       get_pointer(std::shared_ptr<T>& p)       { return p.get(); }
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
                SunSky const &sunSky,
                std::shared_ptr<excyrender::Primitives::Primitive> primitive)
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

        auto const integrator =
                SurfaceIntegrators::Path(
                    surface_integrator.maximum_recursion,
                    *primitive,
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
    // TODO: rid this class once we have a generic Color class.
    struct PyConstantTexture final : excyrender::Photometry::Texture::SpectrumTexture
    {
        PyConstantTexture (excyrender::Photometry::RGB const &rgb) :
            val(excyrender::Photometry::Spectrum::FromRGB(400,800,8,rgb))
        {}

        excyrender::Photometry::Spectrum operator() (excyrender::DifferentialGeometry const &dg) const noexcept {
            return val(dg);
        }
    private:
        excyrender::Photometry::Texture::ConstantTexture<excyrender::Photometry::Spectrum> val;
    };

    using namespace boost::python;
    using namespace PyAPI;

    using excyrender::real;
    using excyrender::Geometry::Vector;
    using excyrender::Photometry::Spectrum;
    using namespace excyrender::Photometry::Texture;
    using namespace excyrender::Photometry;


    // Mappings -----------------------------------------------------------------------------------
    class_<UVMapping2d, std::shared_ptr<UVMapping2d>, Mapping2d>
      ("UVMapping2d", init<real,real,real,real>((arg("scale_u"), arg("scale_v"), arg("offset_u")=0, arg("offset_v")=0)));

    class_<PlanarMapping2d, std::shared_ptr<PlanarMapping2d>, Mapping2d>
      ("PlanarMapping2d", init<Vector,Vector,real,real>((arg("vs"), arg("vt"), arg("offset_u")=0, arg("offset_v")=0)));

    implicitly_convertible<std::shared_ptr<UVMapping2d>,     std::shared_ptr<Mapping2d> >();
    implicitly_convertible<std::shared_ptr<PlanarMapping2d>, std::shared_ptr<Mapping2d> >();


    // Textures -----------------------------------------------------------------------------------
    class_<ColorImageTexture, std::shared_ptr<ColorImageTexture>, boost::noncopyable>
      ("ImageTexture", init<std::shared_ptr<Mapping2d>, std::string>((arg("mapping"), arg("filename"))));

    class_<PyConstantTexture, std::shared_ptr<PyConstantTexture>, boost::noncopyable>
      ("ConstantTexture", init<RGB>());

    implicitly_convertible<std::shared_ptr<ColorImageTexture>, std::shared_ptr<SpectrumTexture> >();
    implicitly_convertible<std::shared_ptr<PyConstantTexture>, std::shared_ptr<SpectrumTexture> >();


    // Materials ----------------------------------------------------------------------------------
    class_<Material::Lambertian, std::shared_ptr<Material::Lambertian>, Material::Material>
      ("Lambertian", init<std::shared_ptr<SpectrumTexture>>((arg("texture"))));

    implicitly_convertible<std::shared_ptr<Material::Lambertian>, std::shared_ptr<Material::Material> >();
}



void geometry_api()
{
    using namespace boost::python;
    using namespace PyAPI;
    using excyrender::real;
    using namespace excyrender::Geometry;

    // Direction
    Direction (*direction)(real,real,real) = &excyrender::Geometry::direction;
    def("direction", direction);

    class_<Direction>
      ("Direction", init<Direction>())
      .def("x", &Direction::x)
      .def("y", &Direction::y)
      .def("z", &Direction::z);

    // Vector
    class_<Vector>
      ("Vector")
      .def(init<real,real,real>())
      .def_readwrite("x", &Vector::x)
      .def_readwrite("y", &Vector::y)
      .def_readwrite("z", &Vector::z)
      .def(self + self)
      .def(self - self)
      .def(self * real())
      .def(real() * self)
      .def(self / real())
      .def(self_ns::str(self_ns::self))
    ;

    real   (*vec_dot)(Vector const &, Vector const &) = &dot;
    Vector (*vec_normalize)(Vector const &) = &normalize;

    def("dot",               vec_dot);
    def("len_sq",            &excyrender::Geometry::len_sq);
    def("len",               &excyrender::Geometry::len);
    def("normalize",         vec_normalize);
    def("cross",             &excyrender::Geometry::cross);
    def("create_orthogonal", &excyrender::Geometry::createOrthogonal);


    // Point2d
    class_<Point2d>
      ("Point2d")
      .def(init<real,real>())
      .def_readwrite("x", &Point2d::x)
      .def_readwrite("y", &Point2d::y);

    // Rectangle
    class_<Rectangle>
      ("Rectangle", init<Point2d, Point2d>((arg("min"), arg("max"))))
      .def("left",   &Rectangle::left)
      .def("top",    &Rectangle::top)
      .def("width",  &Rectangle::width)
      .def("height", &Rectangle::height);

}



void rendering_api()
{
    using namespace boost::python;
    using namespace PyAPI;

    def("render", render, (arg("renderer"), arg("sunsky"), arg("primitive")));

    class_<Renderer>("Renderer", init<int, int, int>((arg("width")=640, arg("height")=480, arg("samples_per_pixel")=16)))
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



void primitives_api()
{
    using namespace boost::python;
    using namespace PyAPI;
    using namespace excyrender::Primitives;

    struct local {
        static
        std::shared_ptr<excyrender::Primitives::FinitePrimitive> create_terrain2d_alpha (
            excyrender::Geometry::Rectangle const &world_rect,
            int grid_resolution,
            boost::python::object height_function,
            std::shared_ptr<excyrender::Photometry::Material::Material> material)
        {
            auto ptr = height_function.ptr();
            Py_INCREF(ptr);
            return excyrender::Primitives::create_terrain2d_alpha(world_rect,
                                                                  grid_resolution,
                                                                  excyrender::Scripting::Python::PyHeightFun(ptr),
                                                                  material);
        }
    };

    class_<FinitePrimitive, std::shared_ptr<FinitePrimitive>, boost::noncopyable, Primitive>
      ("FinitePrimitive", no_init);

    class_<Primitive, std::shared_ptr<Primitive>, boost::noncopyable>
      ("Primitive", no_init);

    implicitly_convertible<std::shared_ptr<FinitePrimitive>, std::shared_ptr<Primitive> >();

    def("Terrain2dAlpha", &local::create_terrain2d_alpha,
      (arg("world_rect"), arg("grid_resolution"), arg("height_function"), arg("material")));
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
    primitives_api();
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
