// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef SPECTRUM_HH_INCLUDED_20130712
#define SPECTRUM_HH_INCLUDED_20130712

#include "real.hh"
#include "Photometry/SPD/SPD.hh"
#include "Photometry/SPD/Regular.hh"
#include "Photometry/RGB.hh"
#include "Photometry/CIEMatchingCurves.hh"
#include <tuple>
#include <valarray>
#include <sstream>
#include <stdexcept>

namespace excyrender { namespace Photometry {

    class Spectrum final {
    public:
    
        // -- ctors ---------------------------------------------------------------------
        template <typename Cont>
        Spectrum (real lambdaMin, real lambdaMax, Cont const &spectrum) :
            lambdaMin_   (lambdaMin),
            lambdaMax_   (lambdaMax),
            bins_        (spectrum.size()),
            delta_       ((lambdaMax_ - lambdaMin_) / (spectrum.size()-1)),
            inverseDelta_(1 / delta_)
        {                
            for (int i=0, s=spectrum.size(); i!=s; ++i) {
                bins_[i] = spectrum[i];
            }
        }

        // -- ctors ---------------------------------------------------------------------
        static Spectrum FromSPD(real lambdaMin, real lambdaMax, int resolution, SPD::SPD const &);
        static Spectrum FromSpectrum(real lambdaMin, real lambdaMax, int resolution, Spectrum const &);
        static Spectrum FromRGB(real lambdaMin, real lambdaMax, int resolution, RGB const &rgb);
        static Spectrum Gray   (real lambdaMin, real lambdaMax, int resolution, real g);
        static Spectrum Black  (real lambdaMin, real lambdaMax, int resolution);

        // -- operators -----------------------------------------------------------------        
        Spectrum operator+= (Spectrum const &rhs);
        Spectrum operator-= (Spectrum const &rhs);
        Spectrum operator*= (Spectrum const &rhs);
        Spectrum operator*= (real rhs);
        Spectrum operator/= (real rhs);
        Spectrum pow(real exponent) const;
        
        // -- conversion ----------------------------------------------------------------        
        std::tuple<real,real,real> toXYZ() const;
        real toY() const;

    private:
        real operator() (real lambda) const;

        void assert_topology(Spectrum const &rhs) {
            if (rhs.lambdaMin_ != lambdaMin_
              || rhs.lambdaMax_ != lambdaMax_
              || rhs.bins_.size() != bins_.size())
            {
                std::stringstream ss;
                ss << "Tried to operate on Spectrums of different topology ("
                   << "[" << lambdaMin_ << " " << lambdaMax_ << " " << bins_.size() << "]"
                   << " vs. "
                   << "[" << rhs.lambdaMin_ << " " << rhs.lambdaMax_ << " " << rhs.bins_.size() << "]"
                   << ")";
                throw std::runtime_error(ss.str());
            }
        }
        
    private:
        real lambdaMin_, lambdaMax_, delta_, inverseDelta_;
        std::valarray<real> bins_;
    };
    
    
    //-- ctor -------------------------------------------------------------------------------------
    inline Spectrum Spectrum::FromSPD(real lambdaMin, real lambdaMax, int resolution, SPD::SPD const &spd) {
        const auto range = lambdaMax - lambdaMin,
                   delta = range / (resolution-1);
        std::valarray<real> spec(resolution);
        for (int i=0; i!=resolution; ++i) {
            spec[i] = spd(i / real(resolution) * range + lambdaMin);
        }
        return Spectrum(lambdaMin, lambdaMax, spec);
    }

    inline Spectrum Spectrum::FromSpectrum(real lambdaMin, real lambdaMax, int resolution, Spectrum const &spec) {
        const auto range = lambdaMax - lambdaMin,
                   delta = range / (resolution-1);
        std::valarray<real> bins(resolution);
        for (int i=0; i!=resolution; ++i) {
            bins[i] = spec(i / real(resolution) * range + lambdaMin);
        }
        return Spectrum(lambdaMin, lambdaMax, bins);
    }
        
    inline Spectrum Spectrum::FromRGB(real lambdaMin, real lambdaMax, int resolution, RGB const &rgb) {
        return FromSPD (lambdaMin, lambdaMax, resolution, SPD::Regular::FromRGB(rgb));
    }
    
    inline Spectrum Spectrum::Gray(real lambdaMin, real lambdaMax, int resolution, real g) {
        return FromRGB(lambdaMin, lambdaMax, resolution, RGB(g,g,g));
    }
    
    inline Spectrum Spectrum::Black(real lambdaMin, real lambdaMax, int resolution) {
        return Gray(lambdaMin, lambdaMax, resolution, 0);
    }
    
    
    // -- operators -------------------------------------------------------------------------------
    inline Spectrum Spectrum::operator+= (Spectrum const &rhs) {
        assert_topology(rhs);
        bins_ += rhs.bins_;
        return *this;
    }
    inline Spectrum Spectrum::operator-= (Spectrum const &rhs) {
        assert_topology(rhs);
        bins_ -= rhs.bins_;
        return *this;
    }
    inline Spectrum Spectrum::operator*= (Spectrum const &rhs) {
        assert_topology(rhs);
        bins_ *= rhs.bins_;
        return *this;
    }
    inline Spectrum Spectrum::operator*= (real rhs) {
        bins_ *= rhs;
        return *this;
    }
    inline Spectrum Spectrum::operator/= (real rhs) {
        bins_ /= rhs;
        return *this;
    }
    inline Spectrum Spectrum::pow(real exponent) const {
        Spectrum ret = *this;
        ret.bins_ = std::pow(bins_, exponent);
        return ret;
    }
    
    inline real Spectrum::operator() (real lambda) const {
        const real x = (lambda - lambdaMin_) * inverseDelta_;
        const size_t b0 = std::floor(x),
                     b1 = std::min (b0+1, bins_.size()-1);
        const real dx = x - b0;
        return (1-dx) * bins_[b0] + dx * bins_[b1];
    }



    // -- conversion ------------------------------------------------------------------------------
    inline std::tuple<real,real,real> Spectrum::toXYZ() const {
        using namespace CIEMatchingCurves;
        const auto &samples = FromSpectrum(lambdaMin_, lambdaMax_, cie_length, *this).bins_;
        return std::make_tuple(cie_inverse_length * (cie_x * samples).sum(),
                               cie_inverse_length * (cie_y * samples).sum(),
                               cie_inverse_length * (cie_z * samples).sum());
    }

    inline real Spectrum::toY() const {
        using namespace CIEMatchingCurves;
        const auto &samples = FromSpectrum(lambdaMin_, lambdaMax_, cie_length, *this).bins_;
        return cie_inverse_length * (cie_y * samples).sum();
    }
    
    
    
    //-- free operators ---------------------------------------------------------------------------
    inline Spectrum operator+ (Spectrum lhs, Spectrum const &rhs) { return lhs += rhs; }
    inline Spectrum operator- (Spectrum lhs, Spectrum const &rhs) { return lhs -= rhs; }    
    inline Spectrum operator* (Spectrum lhs, Spectrum const &rhs) { return lhs *= rhs; }    
    inline Spectrum operator* (Spectrum lhs, real rhs) { return lhs *= rhs;  }
    inline Spectrum operator/ (Spectrum lhs, real rhs) { return lhs /= rhs;  }
    inline Spectrum pow       (Spectrum lhs, real exponent) { return lhs.pow(exponent); }
    
    inline Spectrum sum(std::initializer_list<Spectrum> const &spectra) {
        Spectrum ret (Spectrum::Black(300,830,54));
        for (auto s : spectra)
            ret += s;
        return ret;
    }   


} }

#endif

