// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "real.hh"
#include "Photometry/SPD/SPD.hh"

namespace excyrender {
    namespace Photometry {
        namespace SPD {

            class Constant final  : public SPD {
            public:
                Constant() = delete;
                Constant(real lambda_min, real lambda_max, real c)
                    : SPD(lambda_min, lambda_max), c_(c) {}

                real operator() (real) const { return c_; }

                std::tuple<real,real,real> toXYZ() const;

            private:
                real c_;
            };
        }
    }
}

