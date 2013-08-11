// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef SCALETEXTURE_HH_INCLUDED_20130811
#define SCALETEXTURE_HH_INCLUDED_20130811

#include "Texture.hh"
#include "memory.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    template <typename LHS, typename RHS>
    struct ScaleTexture final : Texture<decltype( *((LHS*)nullptr) * *((RHS*)nullptr) )>
    {
        using product_type = decltype( *((LHS*)nullptr) * *((RHS*)nullptr) );


        ScaleTexture (shared_ptr<LHS> const &lhs, shared_ptr<RHS> const &rhs)
            : lhs(lhs), rhs(rhs)
        {}

        product_type operator() (DifferentialGeometry const &dg) const noexcept
        {
            return (*lhs)(dg) * (*rhs)(dg);
        }

    private:
        shared_ptr<LHS> lhs;
        shared_ptr<RHS> rhs;
    };

} } }

#endif // SCALETEXTURE_HH_INCLUDED_20130811
