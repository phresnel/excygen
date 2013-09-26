#include <boost/python.hpp>
#include <libnoise/noise.h>


// g++ --std=c++11  -g -I/usr/include/python2.7 -lboost_python -lnoise -lpthread -ldl -lutil -lm -lpython2.7  -fPIC -shared  -opy_bindings.so PyNoiseBinding.cc

// Some cleaned up bindings.
class RidgedMulti {
public:
    RidgedMulti() {}

    double operator() (double u, double v) const {
        return impl.GetValue(u, v, 0);
    }

    double call(double u, double v) const { return (*this)(u,v); }

private:
    noise::module::RidgedMulti impl;
};

BOOST_PYTHON_MODULE(py_bindings) {
    using namespace boost::python;
    class_<RidgedMulti>("RidgedMulti")
            .def("__call__", &RidgedMulti::operator());
}

