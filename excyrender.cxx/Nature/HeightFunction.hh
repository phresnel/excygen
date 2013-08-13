// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef HEIGHTFUNCTION_HH_INCLUDED_20130813
#define HEIGHTFUNCTION_HH_INCLUDED_20130813

#include "real.hh"
#include <functional>

namespace excyrender { namespace Nature {

    using HeightFunction = std::function<real (real, real)>;

} }

#endif // HEIGHTFUNCTION_HH_INCLUDED_20130813
