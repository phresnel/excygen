// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef BXDF_HH_INCLUDED_201310718
#define BXDF_HH_INCLUDED_201310718

#include "Geometry/Direction.hh"
#include "Photometry/Spectrum.hh"
#include <tuple>

namespace excyrender { namespace Photometry { namespace Surface {

class BxDF {
public:
    enum class Distribution {
        Continuous,
        Specular
    };
    enum class ReflectionClass {
        Reflective,
        Transmissive
    };
    
    const Distribution distribution;
    const ReflectionClass reflection;

    BxDF() = delete;
    virtual ~BxDF() {}
    
    virtual real pdf(Geometry::Direction const &wo, Geometry::Direction const &wi) const noexcept = 0;
    virtual Photometry::Spectrum f(Geometry::Direction const &wo, Geometry::Direction const &wi) const noexcept = 0;
    virtual std::tuple<Geometry::Direction, Photometry::Spectrum, real>
         sample_f(Geometry::Direction const &wo, std::function<real()> rng) const noexcept = 0;

protected:        
    BxDF(Distribution d, ReflectionClass r)
        : distribution(d), reflection(r)
    {}
};


class Lambertian final : public BxDF 
{
public:
    Lambertian() = delete;
    
    Lambertian(Photometry::Spectrum const &s) : 
        BxDF(Distribution::Continuous,
             ReflectionClass::Reflective),
        s(s / pi)
    {
    }
    
    real pdf(Geometry::Direction const &wo, Geometry::Direction const &wi) const noexcept 
    {
        return fabs(wi.y()) / pi;
    }
    
    Photometry::Spectrum f(Geometry::Direction const &wo, Geometry::Direction const &wi) const noexcept 
    {
        return s;
    }
    
    std::tuple<Geometry::Direction, Photometry::Spectrum, real>
      sample_f(Geometry::Direction const &wo, std::function<real()> rng) const noexcept
    {
        const auto wi = Geometry::cosineWeightedHemisphere(rng);
        return std::make_tuple(wi, s, pdf(wo,wi));
    }

private:
    Photometry::Spectrum s;
};



class SpecularReflect final : public BxDF 
{
public:
    SpecularReflect() = delete;
    
    SpecularReflect(Photometry::Spectrum const &s) : 
        BxDF(Distribution::Specular,
             ReflectionClass::Reflective),
        s(s)
    {
    }
    
    real pdf(Geometry::Direction const &wo, Geometry::Direction const &wi) const noexcept 
    {
        return 0;
    }
    
    Photometry::Spectrum f(Geometry::Direction const &wo, Geometry::Direction const &wi) const noexcept 
    {
        return Photometry::Spectrum::Black(400,800,8);
    }
    
    std::tuple<Geometry::Direction, Photometry::Spectrum, real>
      sample_f(Geometry::Direction const &wo, std::function<real()>) const noexcept
    {
        const Geometry::Direction rdir {-wo.x(), wo.y(), -wo.z()};
        return std::make_tuple(rdir, s * (1 / fabs(rdir.y())), 1);
    }

private:
    Photometry::Spectrum s;
};

} } }

#endif // BXDF_HH_INCLUDED_201310718
