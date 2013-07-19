// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PATH_HH_INCLUDED_20130719
#define PATH_HH_INCLUDED_20130719

#include "Primitives/Primitive.hh"
#include "Geometry/Ray.hh"
#include "Intersection.hh"
#include "Photometry/Lighting.hh"
#include "Photometry/BSDF/BSDF.hh"
#include "DifferentialGeometry.hh"

namespace excyrender { namespace SurfaceIntegrators {

    class Path {
    public:
        Path (int maxDepth, Primitives::Primitive const &prim, 
              std::vector<std::shared_ptr<Photometry::LightSource>> ls)
            : maxDepth(maxDepth), primitive(prim), lightSources(ls)
        {
        }
        
        Photometry::Spectrum operator() (Geometry::Ray const &ray, std::function<real()> rng) const
        {
            return integrate(0, ray, rng);
        }

    private:
        Photometry::Spectrum integrate (int currDepth, Geometry::Ray const &ray, std::function<real()> rng) const
        {
            using namespace Photometry;
            using namespace Surface;
            using namespace Geometry;
            using std::tuple; using std::get;
           
            if (currDepth >= maxDepth)
                return Spectrum::Black(400,800,8);

            const auto i = primitive.intersect(ray);
            if (!i) 
                return Spectrum::Black(400,800,8);

            const auto wo = -ray.direction;
                       
            const tuple<Direction, Spectrum, real> s = 
                i->bsdf.sample_f(optional<BxDF::Distribution>(), optional<BxDF::ReflectionClass>(), i->dg, wo, rng);
            const auto wi     = get<0>(s);
            const auto r_surf = get<1>(s);
            const auto r_pdf  = get<2>(s);
            
            const auto r_incoming = integrate(currDepth+1, Ray(i->dg.poi,wi), rng);
            const auto reflection = (r_pdf<=0)
                                    ? (Spectrum::Black(400,800,8))
                                    : (r_surf * r_incoming * (dot(static_cast<Normal>(wi), i->dg.nn)/r_pdf));
                                    
            const auto direct = directLighting (lightSources, primitive, *i, wo);            
            return direct + reflection;                                                
        }

    private:    
        int maxDepth;
        Primitives::Primitive const &primitive;
        std::vector<std::shared_ptr<Photometry::LightSource>> lightSources;
    };
} }

#endif // PATH_HH_INCLUDED_20130719

