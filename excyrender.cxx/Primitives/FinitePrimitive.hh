// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef FINITEPRIMITIVE_HH_INCLUDED_20130722
#define FINITEPRIMITIVE_HH_INCLUDED_20130722

#include "Primitive.hh"
#include "AABB.hh"

namespace excyrender { namespace Primitives {

struct FinitePrimitive : Primitive {
    virtual ~FinitePrimitive() {};
    virtual AABB aabb() const noexcept = 0;
};

} }

#endif // FINITEPRIMITIVE_HH_INCLUDED_20130722
