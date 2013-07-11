// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef REGULARSPD_HH_INCLUDED_20130711
#define REGULARSPD_HH_INCLUDED_20130711

/*
import qualified Photometry.SPD.SPD as SPD
import Photometry.CIEMatchingCurves
import Photometry.RGBToSpectrumCurves
import Data.Vector.Unboxed as V
import RealNum
import Photometry.RGB(RGB(..))
*/

#include "Photometry/SPD/SPD.hh"
#include "Photometry/RGB.hh"
#include <algorithm>
#include <valarray>

namespace excyrender {
    namespace Photometry {
        namespace SPD {
        
            class Regular final  : public SPD {
            public:
                Regular() = delete;

                template <typename Cont>
                Regular(real lambdaMin, real lambdaMax, Cont const &spectrum) :
                    lambdaMin_(lambdaMin), lambdaMax_(lambdaMax), spectrum_(spectrum.size()),
                    delta_((lambdaMax_ - lambdaMin_) / (spectrum_.size()-1)),
                    inverseDelta_(1 / delta_)
                {                
                    for (int i=0, s=spectrum.size(); i!=s; ++i) {
                        spectrum_[i] = spectrum[i];
                    }
                }
                static Regular FromRGB(RGB const &rgb);

                real operator() (real) const ;
                std::tuple<real,real,real> toXYZ() const ;
                //std::unique_ptr<SPD> stretch(real f) const ;
                
            private:
                real lambdaMin_, lambdaMax_;
                std::valarray<real> spectrum_;
                real delta_, inverseDelta_;
            };
            
        }
    }
}

#endif // REGULARSPD_HH_INCLUDED_20130711

