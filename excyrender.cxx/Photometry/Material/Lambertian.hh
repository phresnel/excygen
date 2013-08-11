// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef LAMBERTIAN_HH_20130811
#define LAMBERTIAN_HH_20130811

#include "Material.hh"
#include "Photometry/Texture/Texture.hh"
#include "Photometry/BSDF/BSDF.hh"
#include "Photometry/BSDF/BxDF.hh"
#include "memory.hh"

namespace excyrender { namespace Photometry { namespace Material {
        struct Lambertian final : Material {
            Lambertian (shared_ptr<Photometry::Texture::SpectrumTexture> texture)
             : texture(texture) {
            }

            Surface::BSDF bsdf(DifferentialGeometry const &dg) const noexcept {
                using namespace Surface;
                const auto spec = (*texture)(dg);
                return Surface::BSDF({shared_ptr<BxDF>(new Surface::Lambertian(spec))});
            }

        private:
            shared_ptr<Photometry::Texture::Texture<Spectrum>> texture;
        };
} } }

#endif // LAMBERTIAN_HH_20130811
