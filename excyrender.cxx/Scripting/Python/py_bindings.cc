// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include <boost/python.hpp>
#include <libnoise/noise.h>

//--------------------------------------------------------------------------------------------------
// LibNoise: Stripped down versions
//--------------------------------------------------------------------------------------------------
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
    double operator() (double u, double v) const { return impl.GetValue(u, v, 0); }
private:
    noise::module::RidgedMulti impl;
};


//--------------------------------------------------------------------------------------------------
// LibNoise: Bindings (via boost.python)
//--------------------------------------------------------------------------------------------------
BOOST_PYTHON_MODULE(py_bindings) {
    // NOTE: default values as per the official libnoise-defaults
    using namespace boost::python;
    class_<RidgedMulti>("RidgedMulti",
                        init<double,double,int,int>((arg("frequency")=1.0,
                                                     arg("lacunarity")=2.0,
                                                     arg("octave_count")=6,
                                                     arg("seed")=0) ))
            .def("__call__", &RidgedMulti::operator());
}

