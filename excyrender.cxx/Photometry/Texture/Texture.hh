// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef TEXTURE_HH_INCLUDED_20130811
#define TEXTURE_HH_INCLUDED_20130811

#include "DifferentialGeometry.hh"
#include "Photometry/Spectrum.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    template <typename T>
    struct Texture {
        virtual ~Texture() {}
        virtual T operator() (DifferentialGeometry const &) const noexcept = 0;
    };

    using RealTexture     = Texture<real>;
    using SpectrumTexture = Texture<Spectrum>;

} } }

#endif // TEXTURE_HH_INCLUDED_20130811
