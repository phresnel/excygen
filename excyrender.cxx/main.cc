#include <iostream>
#include <vector>
#include <stdexcept>
#include <cassert>

#include "Photometry/RGB.hh"
#include "ImageFormat/PPM.hh"


int main () {
    using namespace excygen;
    std::vector<Photometry::RGB> pixels;
    int s = 256;
    for (int y=0; y!=s; ++y) {
        for (int x=0; x!=s; ++x) {
            pixels.emplace_back(x/float(s), y/float(s), 0);
        }
    }
    ImageFormat::ppm (std::cout, s, s, pixels);
}

