//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Copyright (C) 2010  Sebastian Mach (*1983)
// * mail: phresnel/at/gmail/dot/com
// * http://phresnel.org
// * http://picogen.org
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ifndef AUX_IMAGE_HH_INCLUDED_20100817
#define AUX_IMAGE_HH_INCLUDED_20100817

#include "real.hh"
#include "Photometry/RGB.hh"
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <cmath>
#include <vector>

namespace excyrender { namespace detail {

enum class ImageWrap {
    Wrap,
    Black,
    Clamp
};

class Image {
        std::vector<Photometry::RGB> h;
        std::vector<real> alpha_;
        unsigned int width_, height_;
        ImageWrap wrap;

        static Uint32 getPixel (SDL_Surface *s, unsigned int x, unsigned int y) {
                switch (s->format->BytesPerPixel) {
                case 1: return ((Uint8*)s->pixels)[x + y*s->pitch];
                case 2: return ((Uint16*)s->pixels)[x + y*(s->pitch/2)];
                case 3: {
                        unsigned int a = ((Uint8*)s->pixels)[    3*x + y*s->pitch];
                        unsigned int b = ((Uint8*)s->pixels)[1 + 3*x + y*s->pitch];
                        unsigned int c = ((Uint8*)s->pixels)[2 + 3*x + y*s->pitch];
                        return (c<<16) | (b<<8) | a;
                        }
                case 4: {
                        unsigned int a = ((Uint8*)s->pixels)[    4*x + y*s->pitch];
                        unsigned int b = ((Uint8*)s->pixels)[1 + 4*x + y*s->pitch];
                        unsigned int c = ((Uint8*)s->pixels)[2 + 4*x + y*s->pitch];
                        unsigned int d = ((Uint8*)s->pixels)[3 + 4*x + y*s->pitch];
                        return (d<<24) | (c<<16) | (b<<8) | a;
                        }
                };
                return 0;
        }

        constexpr static int wrap_num(int x, int dim) noexcept {
            return x<0 ? (x + dim * (-x / dim + 1)) % dim
                       : x % dim;
        }
        constexpr static int clamp_num(int x, int dim) noexcept {
            return x < 0 ? 0 : x >= dim ? dim-1 : x;
        }

        /*static_assert(wrap_num(0, 2) == 0, "wrap_num is buggy");
        static_assert(wrap_num(1, 2) == 1, "wrap_num is buggy");
        static_assert(wrap_num(2, 2) == 0, "wrap_num is buggy");
        static_assert(wrap_num(5, 2) == 1, "wrap_num is buggy");
        static_assert(wrap_num(-250, 1000) == 750, "wrap_num is buggy");
        static_assert(wrap_num(-1250, 1000) == 750, "wrap_num is buggy");
        static_assert(wrap_num(-12500, 1000) == 750, "wrap_num is buggy");*/


        Image (Image const &);
        Image & operator = (Image const &) ;
public:
        Image (const std::string &filename, ImageWrap wrap = ImageWrap::Black)
        : width_(0), height_(0), wrap(wrap)
        {
                if (!load (filename))
                        throw std::runtime_error("error while loading " + filename);
        }

        Image () {}

        virtual ~Image () {
        }

        bool load (const std::string &filename) {
                SDL_Surface *image = IMG_Load(filename.c_str());
                if (image) {
                        h.resize(image->w*image->h);
                        alpha_.resize(image->w*image->h);
                        width_ = image->w;
                        height_ = image->h;

                        for (unsigned int y=0; y<height_; ++y)
                        for (unsigned int x=0; x<width_; ++x) {
                                const Uint32 col = getPixel (image, x, y);
                                Uint8 r,g,b,a;
                                SDL_GetRGBA (col, image->format, &r,&g,&b,&a);

                                h[y*width_+x] = Photometry::RGB(
                                                        r/real(255),
                                                        g/real(255),
                                                        b/real(255));
                                alpha_[y*width_+x] = a/real(255);

                        }
                        SDL_FreeSurface(image);
                        return true;
                }
                return false;
        }

        std::vector<Photometry::RGB> const &pixels() const {
            return h;
        }

        unsigned int width() const {
                return width_;
        }

        unsigned int height() const {
                return height_;
        }


        Photometry::RGB at (int x, int y) const {
                if ((x<0) | (x>=(int)width_) | (y<0) | (y>=(int)height_)) {
                        switch (wrap) {
                        case ImageWrap::Black:
                            return Photometry::RGB();
                        case ImageWrap::Wrap:
                            x = wrap_num(x, width_);
                            y = wrap_num(y, height_);
                            break;
                        case ImageWrap::Clamp:
                            x = clamp_num(x, width_);
                            y = clamp_num(y, height_);
                            break;
                        }
                }
                return h[y*width_ + x];
        }

        real alpha_at (int x, int y) const {
                if ((x<0) | (x>=(int)width_) | (y<0) | (y>=(int)height_)) {
                        switch (wrap) {
                        case ImageWrap::Black:
                            return 0;
                        case ImageWrap::Wrap:
                            x = wrap_num(x, width_);
                            y = wrap_num(y, height_);
                            break;
                        case ImageWrap::Clamp:
                            x = clamp_num(x, width_);
                            y = clamp_num(y, height_);
                            break;
                        }
                }
                return alpha_[y*width_ + x];
        }

        Photometry::RGB lerp (real x, real y) const {
                const real
                        wx = x * (width_-1),
                        wy = y * (height_-1);
                const int
                        a = (int)(wx),
                        b = (int)(wy),
                        c = 1 + (int)(wx),
                        d = 1 + (int)(wy);
                const real
                        u = wx - a,
                        v = wy - b;
                return (1-v)*((1-u)*at(a,b) + u*at(c,b))
                        +v*((1-u)*at(a,d) + u*at(c,d)) ;
        }

        real alpha_lerp (real x, real y) const {
                const real
                        wx = x * (width_-1),
                        wy = y * (height_-1);
                const int
                        a = (int)(wx),
                        b = (int)(wy),
                        c = 1 + (int)(wx),
                        d = 1 + (int)(wy);
                const real
                        u = wx - a,
                        v = wy - b;
                return (1-v)*((1-u)*alpha_at(a,b) + u*alpha_at(c,b))
                        +v*((1-u)*alpha_at(a,d) + u*alpha_at(c,d)) ;
        }

        Photometry::RGB cosine (real x, real y) const {
                // as per http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
                const real
                        wx = x * (width_-1),
                        wy = y * (height_-1);
                const int
                        a = (int)(wx),
                        b = (int)(wy),
                        c = 1 + (int)(wx),
                        d = 1 + (int)(wy);
                const real
                        u_ = wx - a,
                        v_ = wy - b,
                        u = .5 * (1 - std::cos (3.1415927 * u_)),
                        v = .5 * (1 - std::cos (3.1415927 * v_))
                        ;
                return (1-v)*((1-u)*at(a,b) + u*at(c,b))
                        +v*((1-u)*at(a,d) + u*at(c,d)) ;
        }

private:
        static Photometry::RGB cubic (Photometry::RGB v0,
                            Photometry::RGB v1,
                            Photometry::RGB v2,
                            Photometry::RGB v3,
                            real x) {
            const Photometry::RGB
                    P = (v3 - v2) - (v0 - v1),
                    Q = (v0 - v1) - P,
                    R = v2 - v0,
                    S = v1
                    ;
            return P*x*x*x + Q*x*x + R*x + S;
        }
public:

        Photometry::RGB cubic (real x, real y) const {
                // as per http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
                const real
                        wx = x * (width_-1),
                        wy = y * (height_-1);
                const int
                        x0 = -1 + (int)(wx),
                        y0 = -1 + (int)(wy),
                        x1 = (int)(wx),
                        y1 = (int)(wy),
                        x2 = 1 + (int)(wx),
                        y2 = 1 + (int)(wy),
                        x3 = 2 + (int)(wx),
                        y3 = 2 + (int)(wy)
                        ;
                const real
                        u = wx - x1,
                        v = wy - y1
                        ;
                const Photometry::RGB
                        a0 = at(x0, y0),
                        a1 = at(x1, y0),
                        a2 = at(x2, y0),
                        a3 = at(x3, y0),

                        b0 = at(x0, y1),
                        b1 = at(x1, y1),
                        b2 = at(x2, y1),
                        b3 = at(x3, y1),

                        c0 = at(x0, y2),
                        c1 = at(x1, y2),
                        c2 = at(x2, y2),
                        c3 = at(x3, y2),

                        d0 = at(x0, y3),
                        d1 = at(x1, y3),
                        d2 = at(x2, y3),
                        d3 = at(x3, y3),

                        a = cubic(a0, a1, a2, a3, u),
                        b = cubic(b0, b1, b2, b3, u),
                        c = cubic(c0, c1, c2, c3, u),
                        d = cubic(d0, d1, d2, d3, u),
                        h = cubic(a, b, c, d, v)
                        ;
                return h;
        }

        Photometry::RGB nearest (real x, real y) const {
                return at((int)(0.5 + x * (width_-1)), (int)(0.5 + y * (height_-1)));
        }
};

} }

#endif // AUX_IMAGE_HH_INCLUDED_20100817
