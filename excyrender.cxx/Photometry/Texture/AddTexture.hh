// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef ADDTEXTURE_HH_INCLUDED_20130811
#define ADDTEXTURE_HH_INCLUDED_20130811

#include "Texture.hh"
#include "memory.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    namespace detail {
        template <typename LHS, typename RHS>
        using addtexture_result_type = decltype(
            (*((LHS*)nullptr)) (*((DifferentialGeometry*)nullptr))
            +
            (*((LHS*)nullptr)) (*((DifferentialGeometry*)nullptr))
        );
    }

    template <typename LHS, typename RHS>
    struct AddTexture final : Texture<detail::addtexture_result_type<LHS,RHS>>
    {
        using result_type = detail::addtexture_result_type<LHS,RHS>;


        AddTexture (shared_ptr<LHS> lhs, shared_ptr<RHS> rhs)
            : lhs(lhs), rhs(rhs)
        {}

        result_type operator() (DifferentialGeometry const &dg) const noexcept
        {
            return (*lhs)(dg) + (*rhs)(dg);
        }

    private:
        shared_ptr<LHS> lhs;
        shared_ptr<RHS> rhs;
    };

} } }

#endif // ADDTEXTURE_HH_INCLUDED_20130811
