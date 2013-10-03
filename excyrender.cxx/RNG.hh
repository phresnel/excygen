// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef RNG_HH
#define RNG_HH

#include "real.hh"
#include <random>
#include <memory>

namespace excyrender {

    class RNG {
    public:
        RNG(uint_fast32_t seed)
            : data(new data_t{std::mt19937(seed),
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

}

#endif // RNG_HH
