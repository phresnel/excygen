// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef LERPTEXTURE_HH_INCLUDED_20130811
#define LERPTEXTURE_HH_INCLUDED_20130811

#include "Texture.hh"
#include "memory.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    namespace detail {
        template <typename LHS, typename RHS, typename FACTOR>
        using lerptexture_result_type = decltype(
            (*((LHS*)nullptr)) (*((DifferentialGeometry*)nullptr))
              * (1-(*((FACTOR*)nullptr)) (*((DifferentialGeometry*)nullptr)))
            +
            (*((LHS*)nullptr)) (*((DifferentialGeometry*)nullptr))
              * (*((FACTOR*)nullptr)) (*((DifferentialGeometry*)nullptr))
        );
    }

    template <typename LHS, typename RHS, typename FACTOR>
    struct LerpTexture final : Texture<detail::lerptexture_result_type<LHS,RHS,FACTOR>>
    {
        using result_type = detail::lerptexture_result_type<LHS,RHS,FACTOR>;


        LerpTexture (shared_ptr<LHS> lhs, shared_ptr<RHS> rhs,
                     shared_ptr<FACTOR> factor)
            : lhs(lhs), rhs(rhs), factor(factor)
        {}

        result_type operator() (DifferentialGeometry const &dg) const noexcept
        {
            const auto &tmp = (*factor)(dg);
            return (*lhs)(dg) * (1-tmp) + (*rhs)(dg) * tmp;
        }

    private:
        shared_ptr<LHS> lhs;
        shared_ptr<RHS> rhs;
        shared_ptr<FACTOR> factor;
    };

} } }

#endif // LERPTEXTURE_HH_INCLUDED_20130811
