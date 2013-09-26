// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include <boost/python.hpp>
#include <libnoise/noise.h>

//--------------------------------------------------------------------------------------------------
// LibNoise: Stripped down versions
//--------------------------------------------------------------------------------------------------
class Billow {
public:
    Billow(double frequency, double lacunarity, int octaveCount, double persistence, int seed)
    {
        impl.SetNoiseQuality(noise::QUALITY_BEST);
        impl.SetFrequency(frequency);
        impl.SetLacunarity(lacunarity);
        impl.SetPersistence(persistence);
        impl.SetOctaveCount(octaveCount);
        impl.SetSeed(seed);
    }
    double operator() (double u, double v) const { return impl.GetValue(u, v, 0.5); }
private:
    noise::module::Billow impl;
};

class Cylinders {
public:
    Cylinders(double frequency) { impl.SetFrequency(frequency); }
    double operator() (double u, double v) const { return impl.GetValue(u, v, 1.0); }
private:
    noise::module::Cylinders impl;
};

class Perlin {
public:
    Perlin(double frequency, double lacunarity, int octaveCount, double persistence, int seed)
    {
        impl.SetNoiseQuality(noise::QUALITY_BEST);
        impl.SetFrequency(frequency);
        impl.SetLacunarity(lacunarity);
        impl.SetPersistence(persistence);
        impl.SetOctaveCount(octaveCount);
        impl.SetSeed(seed);
    }
    double operator() (double u, double v) const { return impl.GetValue(u, v, 0.5); }
private:
    noise::module::Perlin impl;
};

class RidgedMulti {
public:
    RidgedMulti(double frequency, double lacunarity, int octaveCount, int seed)
    {
        impl.SetNoiseQuality(noise::QUALITY_BEST);
        impl.SetFrequency(frequency);
        impl.SetLacunarity(lacunarity);
        impl.SetOctaveCount(octaveCount);
        impl.SetSeed(seed);
    }
    double operator() (double u, double v) const { return impl.GetValue(u, v, 0.5); }
private:
    noise::module::RidgedMulti impl;
};

class Spheres {
public:
    Spheres(double frequency) { impl.SetFrequency(frequency); }
    double operator() (double u, double v) const { return impl.GetValue(u, v, 0.0); }
private:
    noise::module::Spheres impl;
};

//--------------------------------------------------------------------------------------------------
// LibNoise: Bindings (via boost.python)
//--------------------------------------------------------------------------------------------------
BOOST_PYTHON_MODULE(py_bindings) {
    // NOTE: default values as per the official libnoise-defaults
    using namespace boost::python;

    /*class_<Cylinders>
        ("Cylinders", init<double>((arg("frequency")=noise::module::DEFAULT_CYLINDERS_FREQUENCY) ))
        .def("__call__", &Cylinders::operator());*/

    class_<Billow>
        ("Billow", init<double,double,int,double,int>( (arg("frequency")=noise::module::DEFAULT_BILLOW_FREQUENCY,
                                                        arg("lacunarity")=noise::module::DEFAULT_BILLOW_LACUNARITY,
                                                        arg("octave_count")=noise::module::DEFAULT_BILLOW_OCTAVE_COUNT,
                                                        arg("persistence")=noise::module::DEFAULT_BILLOW_PERSISTENCE,
                                                        arg("seed")=noise::module::DEFAULT_BILLOW_SEED) ))
        .def("__call__", &Billow::operator());

    class_<Perlin>
        ("Perlin", init<double,double,int,double,int>( (arg("frequency")=noise::module::DEFAULT_PERLIN_FREQUENCY,
                                                        arg("lacunarity")=noise::module::DEFAULT_PERLIN_LACUNARITY,
                                                        arg("octave_count")=noise::module::DEFAULT_PERLIN_OCTAVE_COUNT,
                                                        arg("persistence")=noise::module::DEFAULT_PERLIN_PERSISTENCE,
                                                        arg("seed")=noise::module::DEFAULT_PERLIN_SEED) ))
        .def("__call__", &Perlin::operator());

    class_<RidgedMulti>
        ("RidgedMulti", init<double,double,int,int>((arg("frequency")=noise::module::DEFAULT_RIDGED_FREQUENCY,
                                                     arg("lacunarity")=noise::module::DEFAULT_RIDGED_LACUNARITY,
                                                     arg("octave_count")=noise::module::DEFAULT_RIDGED_OCTAVE_COUNT,
                                                     arg("seed")=noise::module::DEFAULT_RIDGED_SEED) ))
        .def("__call__", &RidgedMulti::operator());

    /*class_<Spheres>
        ("Spheres", init<double>((arg("frequency")=noise::module::DEFAULT_SPHERES_FREQUENCY) ))
        .def("__call__", &Spheres::operator());*/
}

