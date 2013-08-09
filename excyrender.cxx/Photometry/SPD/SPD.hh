// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef SPD_HH_INCLUDED_20130711
#define SPD_HH_INCLUDED_20130711

#include "real.hh"
#include <tuple>
#include <memory>

namespace excyrender {
    namespace Photometry {
        namespace SPD {

            class SPD {
            public:
                virtual ~SPD() {}

                SPD(real lambda_min, real lambda_max)
                    : lambda_min(lambda_min), lambda_max(lambda_max) {}

                // It is valid to use operator() for arguments outside the lambda-range,
                // outside values are then assumed 0.
                virtual real operator() (real) const = 0;

                virtual std::tuple<real,real,real> toXYZ() const = 0;

                real lambda_min = 0, lambda_max = 0;
            };

        }
    }
}

#endif // SPD_HH_INCLUDED_20130711

