// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef SIMPLE_HH_INCLUDED_20130809
#define SIMPLE_HH_INCLUDED_20130809

#include "Material.hh"

namespace excyrender { namespace Photometry { namespace Material {
        struct BSDFPassthrough final : Material {
            BSDFPassthrough(Surface::BSDF const &bsdf) : bsdf_(bsdf) {}

            Surface::BSDF bsdf(DifferentialGeometry const &) const noexcept {
                return bsdf_;
            }
        private:
            Surface::BSDF bsdf_;
        };
} } }


#endif // SIMPLE_HH_INCLUDED_20130809

