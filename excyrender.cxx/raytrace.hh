// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef RAYTRACE_HH_INCLUDED_20131003
#define RAYTRACE_HH_INCLUDED_20131003

#include "Photometry/Spectrum.hh"
#include "Photometry/RGB.hh"
#include "Geometry/Ray.hh"
#include "DebugPixel.hh"
#include <functional>
#include <memory>

namespace excyrender {
    void raytrace (int width, int height, int samples_per_pixel,
                   std::function<Photometry::Spectrum(Geometry::Ray const &, std::function<float()>)> integrate,
                   std::vector<Photometry::RGB> &pixels,
                   std::vector<DebugPixel> &debug);
}

#endif // RAYTRACE_HH_INCLUDED_20131003
