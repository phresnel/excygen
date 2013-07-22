// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef FINITESHAPE_HH_INCLUDED_20130718
#define FINITESHAPE_HH_INCLUDED_20130718

#include "AABB.hh"
#include "Shape.hh"

namespace excyrender { namespace Shapes {

    class FiniteShape : public Shape {
    public:
        virtual ~FiniteShape() {}
        virtual AABB aabb() const = 0;
    };

} }

#endif // SHAPE_HH_INCLUDED_20130718

