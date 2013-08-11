// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef IMAGETEXTURE_HH
#define IMAGETEXTURE_HH

#include "Texture.hh"
#include "Mapping2d.hh"
#include "detail/Image.hh"

namespace excyrender { namespace Photometry { namespace Texture {

    using excyrender::detail::ImageWrap;

    template <typename T>
    class ImageTexture final : public Texture<T> {
    public:

        ImageTexture(shared_ptr<Mapping2d> mapping,
                     std::string const &filename,
                     ImageWrap wrap = ImageWrap::Wrap)
            : mapping(mapping), wrap(wrap), image(filename, wrap)
        {
        }

        T operator() (DifferentialGeometry const &dg) const noexcept;

    private:
        shared_ptr<Mapping2d> mapping;
        ImageWrap wrap;
        excyrender::detail::Image image;
    };


    template <>
    inline
    real ImageTexture<real>::operator() (DifferentialGeometry const &dg) const noexcept {
        const auto coords = (*mapping)(dg);
        return Spectrum::FromRGB(400,800,8, image.cubic(coords.s, coords.t)).toY(); // TODO: optimize
    }

    template <>
    inline
    Spectrum ImageTexture<Spectrum>::operator() (DifferentialGeometry const &dg) const noexcept {
        const auto coords = (*mapping)(dg);
        return Spectrum::FromRGB(400,800,8, image.cubic(coords.s, coords.t));
    }

} } }

#endif // IMAGETEXTURE_HH
