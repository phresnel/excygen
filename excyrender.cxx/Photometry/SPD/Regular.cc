// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

/*
import qualified Photometry.SPD.SPD as SPD
import Photometry.CIEMatchingCurves
import Photometry.RGBToSpectrumCurves
import Data.Vector.Unboxed as V
import RealNum
import Photometry.RGB(RGB(..))
*/

#include "Photometry/SPD/Regular.hh"
#include "Photometry/CIEMatchingCurves.hh"
#include "Photometry/RGBToSpectrumCurves.hh"
#include "Photometry/RGB.hh"
#include <algorithm>

namespace excyrender {
    namespace Photometry {
        namespace SPD {

            real Regular::operator() (real lambda) const
            {
                const real x = (lambda - lambdaMin_) * inverseDelta_;
                const size_t b0 = std::floor(x),
                             b1 = std::min (b0+1, spectrum_.size()-1);
                const real dx = x - b0;
                return (1-dx) * spectrum_[b0] + dx * spectrum_[b1];
            }
            
            std::tuple<real,real,real> Regular::toXYZ() const
            {
                /*
                let samples = V.map f (V.enumFromN 0 cie_length)
                       where f i = sampleRegular_unchecked lambdaMin spectrum inverseDelta 
                                                           $ lambdaMin + inverseDelta * i 
                in (cie_inverse_length * V.sum (V.zipWith (*) cie_x' samples),
                    cie_inverse_length * V.sum (V.zipWith (*) cie_y' samples),
                    cie_inverse_length * V.sum (V.zipWith (*) cie_z' samples))
                */
                    
                using namespace CIEMatchingCurves;

                std::valarray<real> samples(cie_length);                
                for (int i=0; i!=cie_length; ++i) {
                    samples[i] = (*this)(lambdaMin_ + inverseDelta_ * i);
                }
                return std::make_tuple(
                    cie_inverse_length * (samples * cie_x).sum(),
                    cie_inverse_length * (samples * cie_x).sum(),
                    cie_inverse_length * (samples * cie_x).sum()
                );
            }

            /*std::unique_ptr<SPD> Regular::stretch(real f) const {
               // SPD.stretch = \f -> regularSPD lambdaMin' lambdaMax' $ V.toList $ V.map (f*) spectrum'
               //return new Regular
            }*/
            
            
            Regular Regular::FromRGB(RGB const &rgb)
            {
                using namespace RGBToSpectrumCurves;

                const real r = rgb.r, g = rgb.g, b = rgb.b;

                std::valarray<real> u(rgbToSpectrumCurves_length),
                                    v(rgbToSpectrumCurves_length),
                                    w(rgbToSpectrumCurves_length);                
                if (r<=g && r<=b)
                {
                    // Compute reflectance _SampledSpectrum_ with _r_ as minimum
                    u = rgbRefl2SpectWhite * r;          // -- r += r * rgbRefl2SpectWhite                
                    if (g<=b) {
                        v = rgbRefl2SpectCyan * (g-r);   // -- r += (g - r) * rgbRefl2SpectCyan;
                        w = rgbRefl2SpectBlue * (b-g);   // -- r += (b - g) * rgbRefl2SpectBlue;
                    } else {
                        v = rgbRefl2SpectCyan  * (b-r);  // -- r += (b - r) * rgbRefl2SpectCyan;
                        w = rgbRefl2SpectGreen * (g-b);  // -- r += (g - b) * rgbRefl2SpectGreen;
                    }
                }
                else if (g<=r && g<=b)
                {
                    // -- Compute reflectance _SampledSpectrum_ with _g_ as minimum
                    u = rgbRefl2SpectWhite * g;           // -- r += g * rgbRefl2SpectWhite                
                    if (r<=b) {
                        v = rgbRefl2SpectMagenta * (r-g); // -- r += (r - g) * rgbRefl2SpectMagenta
                        w = rgbRefl2SpectBlue    * (b-r); // -- r += (b - r) * rgbRefl2SpectBlue
                    } else {
                        v = rgbRefl2SpectMagenta * (b-g); // -- r += (b - g) * rgbRefl2SpectMagenta
                        w = rgbRefl2SpectRed     * (r-b); // -- r += (r - b) * rgbRefl2SpectRed
                    }
                }
                else
                {
                    // -- Compute reflectance _SampledSpectrum_ with _b_ as minimum
                    u = rgbRefl2SpectWhite * b;          // -- r += b * rgbRefl2SpectWhite;
                    if (r<=b) {
                        v = rgbRefl2SpectYellow * (r-b); // -- r += (r - b) * rgbRefl2SpectYellow;
                        w = rgbRefl2SpectGreen  * (g-r); // -- r += (g - r) * rgbRefl2SpectGreen;
                    } else {
                        v = rgbRefl2SpectYellow * (g-b); // -- r += (g - b) * rgbRefl2SpectYellow;
                        w = rgbRefl2SpectRed    * (r-g); // -- r += (r - g) * rgbRefl2SpectRed;
                    }                    
                }
                // TODO: check what this 0.94 is for
                return Regular(rgbToSpectrumCurves_start, rgbToSpectrumCurves_end, (u+v+w)*0.94);
            }
        }
    }
}

