// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#ifndef BSDF_HH_INCLUDED_20130718
#define BSDF_HH_INCLUDED_20130718

#include "Photometry/Spectrum.hh"
#include "Photometry/BSDF/BxDF.hh"
#include "DifferentialGeometry.hh"
#include "Geometry/Direction.hh"
#include "optional.hh"
#include <vector>
#include <memory>
#include <initializer_list>


namespace excyrender { namespace Photometry { namespace BSDF {

    class BSDF final
    {
    public:
        BSDF() = delete;
        BSDF(std::initializer_list<std::shared_ptr<BxDF>> const &l)
            : bxdfs(bxdfs)
        {
            if (!l.size())
                throw std::logic_error("BSDF must have one or more BxDFs");
        }
        
        real pdf(Geometry::Direction const &wo, Geometry::Direction const &wi) const noexcept
        {
            real sum = 0;
            for (auto const &bxdf_ : bxdfs) {
                auto const &bxdf = *bxdf_;
                if (bxdf.distribution == BxDF::Distribution::Continuous)
                    sum += bxdf.pdf(wo, wi);
            }
            return sum;
        }
        
        Photometry::Spectrum 
         f(Geometry::Direction const &wo, Geometry::Direction const &wi) 
         const noexcept
        {
            auto sum = Photometry::Spectrum::Black(400,800,8);
            for (auto const &bxdf_ : bxdfs) {
                auto const &bxdf = *bxdf_;
                if (bxdf.distribution == BxDF::Distribution::Continuous)
                    sum += bxdf.f(wo, wi);
            }
            return sum;
        }

        std::tuple<Geometry::Direction, Photometry::Spectrum, real>
         sample_f(optional<BxDF::Distribution> dist, 
                  optional<BxDF::ReflectionClass> refl,
                  DifferentialGeometry const &dg,
                  Geometry::Direction const &wo,
                  std::function<real()> rng
                 ) const noexcept
        {
            int count = 0;
            for (auto const &bxdf : bxdfs) {
                if (!dist || *dist==bxdf->distribution
                  && !refl || *refl==bxdf->reflection)
                  ++count;
            }
            if (count != 1)
                throw std::logic_error("BSDF currently supports up tp 1 sample-able BxDFs");
            if (count != 0) {
                const auto wo_ = worldToLocal(dg, wo);
                for (auto const &bxdf_ : bxdfs) {
                    auto const &bxdf = *bxdf_;
                    if (!dist || *dist==bxdf.distribution
                      && !refl || *refl==bxdf.reflection)
                    {
                        const auto & r = bxdf.sample_f(wo_, rng);
                        return std::make_tuple(localToWorld(dg, std::get<0>(r)),
                                               std::get<1>(r),
                                               std::get<2>(r));
                    }
                }
            }
            return std::make_tuple(Geometry::Direction(0,1,0),
                                   Photometry::Spectrum::Black(400,800,8),
                                   0);
        }

    private:        
        std::vector<std::shared_ptr<BxDF>> bxdfs;
    };
} } }

#endif // BSDF_HH_INCLUDED_20130718

