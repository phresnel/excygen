// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef CONSTANTTEXTURE_HH_INCLUDED_20130811
#define CONSTANTTEXTURE_HH_INCLUDED_20130811

#include "Texture.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    template <typename T>
    struct ConstantTexture final : Texture<T>
    {
        ConstantTexture (T const &val) : val(val) {}

        T operator() (DifferentialGeometry const &) const noexcept {
            return val;
        }

    private:
        T val;
    };

} } }

#endif // CONSTANTTEXTURE_HH_INCLUDED_20130811
