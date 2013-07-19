// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "ImageFormat/PPM.hh"
#include "Photometry/RGB.hh"
#include <ostream>
#include <stdexcept>

namespace excyrender { namespace ImageFormat {

    void ppm (std::ostream &os, int width, int height, std::vector<Photometry::RGB> const &pixels) {
        if (width * height != pixels.size())
            throw std::runtime_error ("ImageFormat::ppm: width*height is inequal to number of pixels passed");
        os << "P3\n"
           << width << ' ' << height << '\n'
           << "255\n";
        for (int y=0; y<height; ++y) {
            for (int x=0; x<width; ++x) {
                auto rgb = saturate (pixels[y*width+x]*255, 0, 255);
                os << static_cast<int>(rgb.r) << ' '
                   << static_cast<int>(rgb.g) << ' '
                   << static_cast<int>(rgb.b) << ' ';
                
                //auto rgb = pixels[y*width+x]*255;//saturate (pixels[y*width+x]*255, 0, 255);
                //os << (((rgb.r+rgb.g+rgb.b)/3)>0.5) << ' ';
            }
            os << '\n';
        }
    }

} }
