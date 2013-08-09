// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#ifndef LIGHTING_HH_INCLUDED_20130712
#define LIGHTING_HH_INCLUDED_20130712

#include "DifferentialGeometry.hh"
#include "Geometry/Direction.hh"
#include "Geometry/Point.hh"
#include "Geometry/Normal.hh"
#include "Intersection.hh"
#include "Photometry/BSDF/BSDF.hh"
#include "Primitives/Primitive.hh"
#include "Photometry/Spectrum.hh"

namespace excyrender { namespace Photometry {

    class LightSource {
    public:
        virtual ~LightSource() {}
        virtual Photometry::Spectrum
            lightFrom (Geometry::Direction const &wo, Surface::BSDF const &bsdf,
                       Primitives::Primitive const &prim,
                       Geometry::Point const &at, Geometry::Normal const &n) const noexcept = 0;
    };

    class Directional final : public LightSource {
    public:
        Directional() = delete;

        Directional(Geometry::Direction const &wi, Photometry::Spectrum const &color)
            : wi(wi), color(color)
        {}

        Photometry::Spectrum
           lightFrom (Geometry::Direction const &wo, Surface::BSDF const &bsdf,
                      Primitives::Primitive const &prim,
                      Geometry::Point const &at, Geometry::Normal const &n) const noexcept
        {
            const real transmittance = prim.occludes(at, wi) ? 0 : 1,
                       dot_ = max(real(0), dot(static_cast<Geometry::Direction>(n), wi));
            return bsdf.f(wo, wi) * color * (dot_*transmittance);
        }

    private:
        Geometry::Direction wi;
        Photometry::Spectrum color;
    };


    template <typename Lights>
    Spectrum directLighting(Lights const &lights,
                            Primitives::Primitive const &prim,
                            Intersection const &intersection,
                            Geometry::Direction const &wo) noexcept
    {
        Photometry::Spectrum sum = Photometry::Spectrum::Black(400, 800, 8);
        for (auto light : lights) {
            sum += light->lightFrom(wo, intersection.material->bsdf(intersection.dg),
                                    prim, intersection.dg.poi, intersection.dg.nn);
        }
        return sum;
    }

} }

#endif // LIGHTING_HH_INCLUDED_20130712

