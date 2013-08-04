//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Copyright (C) 2010 Sebastian Mach (*1983)
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

#ifndef KISS_HH_INCLUDED_20100301
#define KISS_HH_INCLUDED_20100301

// Implemented after:
// [0] http://www.bobwheeler.com/statistics/Password/MarsagliaPost.txt
// [1a] http://www.math.niu.edu/~rusin/known-math/99/RNG
// [1b] http://groups.google.com/group/sci.crypt/browse_thread/thread/...
//      ...ca8682a4658a124d/   (glue at ... parts and remove ...)
//
// Note that Marsaglias first post [0] had some flaws. E.g., in Wnew, the
// result was &'ed with 65535, in the end causing that the second seed to KISS
// had only a very small effect to the sequence, meaning in UNI, that sequences
// differed by only a small fraction (like 0.02345678 vs. 0.234569).
//
// This version now represents Marsaglias generators from his second post
// [1a,1b].
//

#include <inttypes.h>

namespace portable_rng { namespace marsaglia {

        // -- Bits ----------------------------------------------------
        struct Znew {
                explicit Znew (uint32_t value=362436069) : z(value) {}
                Znew & operator = (uint32_t value) {
                        z = value;
                        return *this;
                }
                uint32_t operator () () { return znew(); }
        protected:
                uint32_t znew() {
                        // orig: (z=36969*(z&65535)+(z>>16))

                        const uint32_t
                          alpha = z&65535,
                          bravo = 36969*alpha,
                          charlie = z>>16
                        ;
                        z = bravo + charlie;

                        // Note that the maximum of z does not yield
                        // the maximum return value, as for the left
                        // shift. The maximum return value is reached
                        // for all z that have the lower 16 bits set.
                        return z;

                }
        private:
                uint32_t z;
        };

        struct Wnew {
                explicit Wnew (uint32_t value=521288629) : w(value) {}
                Wnew & operator = (uint32_t value) {
                        w = value;
                        return *this;
                }
                uint32_t operator () () { return wnew(); }
        protected:
                uint32_t wnew() {
                        // orig: (w=18000*(w&65535)+(w>>16))


                        // All maxima are in uint32_t bounds,
                        // hence behaviour should be well-defined.
                        const uint32_t
                          alpha = w&65535,
                          bravo = 18000*alpha,
                          charlie = w>>16
                        ;
                        w = bravo + charlie;
                        return w;
                }
        private:
                uint32_t w;
        }; // okay

        // ------------------------------------------------------------



        // -- Basically, safe typedefs to uint --------------------------------
        struct Jsr {
                explicit Jsr (uint32_t value=123456789) : jsr(value) {}
                Jsr & operator = (uint32_t value) {
                        jsr = value;
                        return *this;
                }
                operator uint32_t () const {
                        return jsr;
                }
        private:
                uint32_t jsr;
        };

        struct Jcong {
                explicit Jcong (uint32_t value=380116160) : jcong(value) {}
                Jcong & operator = (uint32_t value) {
                        jcong = value;
                        return *this;
                }
                operator uint32_t () const {
                        return jcong;
                }
        private:
                uint32_t jcong;
        };
        // --------------------------------------------------------------------



        // -- Aggregates ------------------------------------------------------
        class MWC {
        public:
                MWC (Znew const & znew_, Wnew const & wnew_)
                : znew(znew_), wnew(wnew_)
                {}
                explicit MWC (uint32_t znew_, uint32_t wnew_)
                : znew(znew_), wnew(wnew_)
                {}
                MWC () {}

                uint32_t operator () () { return mwc(); }

                void skip (uint32_t count) {
                        for (uint32_t u=0; u<count; ++u)
                                mwc();
                }
        protected:
                uint32_t mwc () {
                        // orig: ((znew<<16)+wnew )
                        return (znew()<<16) + wnew();
                }
        private:
                Znew znew;
                Wnew wnew;
        };

        class SHR3 {
        public:
                SHR3 (Jsr const &jsr_=Jsr()) : jsr(jsr_) {}
                explicit SHR3 (uint32_t jsr_) : jsr(jsr_) {}

                uint32_t operator () () { return shr3(); }
        protected:
                uint32_t shr3 () {
                        // (jsr^=(jsr<<17), jsr^=(jsr>>13), jsr^=(jsr<<5))
                        jsr = jsr ^ static_cast<uint32_t>(jsr<<17);
                        jsr = jsr ^ static_cast<uint32_t>(jsr>>13);
                        jsr = jsr ^ static_cast<uint32_t>(jsr<<5);
                        return jsr;
                }
        private:
                Jsr jsr;
        };

        class CONG  {
        public:
                CONG (Jcong const &jcong_=Jcong()) : jcong(jcong_) {}
                explicit CONG (uint32_t jcong_) : jcong(jcong_) {}
                uint32_t operator () () { return cong(); }
        protected:
                uint32_t cong () {
                        // orig: (jcong=69069*jcong+1234567)
                        return jcong = static_cast<uint32_t>(
                                static_cast<uint32_t>(69069 * jcong)
                                + 1234567
                        );
                }
        private:
                Jcong jcong;
        };

        class KISS : MWC, CONG, SHR3 {
        public:
                /*KISS(MWC const&mwc_, CONG const&cong_, SHR3 const&shr3_)
                : MWC(mwc_), CONG(cong_), SHR3(shr3_)
                {}*/
                explicit KISS(
                        uint32_t mwc_znew, uint32_t mwc_wnew,
                        uint32_t cong,
                        uint32_t shr3
                ) : mwc(mwc_znew, mwc_wnew)
                , cong(cong)
                , shr3(shr3)
                {}

                KISS () {}
                uint32_t operator () () { return kiss(); }
                void skip (uint32_t count) {
                        for (uint32_t u=0; u<count; ++u)
                                kiss();
                }
        protected:
                uint32_t kiss () {
                        // orig: ((MWC^CONG)+SHR3)
                        return (mwc()^cong())+shr3();
                }
        private:
                MWC mwc;
                CONG cong;
                SHR3 shr3;
        };
        // --------------------------------------------------------------------


        // -- Distributions ---------------------------------------------------
        // I think UNI should be a distribution, not an RNG on itself
        class UNI {
        public:
                //UNI () {}
                //UNI (KISS const &kiss) : KISS(kiss) {}

                explicit UNI(
                        uint32_t mwc_znew, uint32_t mwc_wnew,
                        uint32_t cong,
                        uint32_t shr3
                ) : kiss(mwc_znew, mwc_wnew, cong, shr3)
                {}

                double operator () () { return uni(); }
                void skip (uint32_t count) { kiss.skip(count); }
        protected:
                double uni () {
                        return kiss() * 2.328306e-10;
                }
        private:
                KISS kiss;
        };

        // I think VNI should be a distribution, not an RNG on itself
        class VNI  {
        public:
                //VNI () {}
                //VNI (KISS const &kiss) : KISS(kiss) {}
                explicit VNI(
                        uint32_t mwc_znew, uint32_t mwc_wnew,
                        uint32_t cong,
                        uint32_t shr3
                ) : kiss(mwc_znew, mwc_wnew, cong, shr3)
                {}
                double operator () () { return vni(); }
                void skip (uint32_t count) { kiss.skip(count); }
        protected:
                double vni () {
                        // The original by Marsaglia converted kiss() to
                        // a signed integer instead of subtracting 1.0.
                        //----------------------------------------------
                        // C++-03, section 5.5:
                        // "If during  the  evaluation of an expression,
                        //  the result is not mathematically  defined or
                        //  not in the range of representable values for
                        //  its type, the behavior is undefined,  unless
                        //  such an expression is a constant expression"
                        // C++-03, section 4.7. Integral Conversion:
                        //  3) If  the  destination type is signed,  the
                        //     value is  unchanged  if it can be  repre-
                        //     sented in the destination type  (and bit-
                        //     field width);  otherwise,  the  value  is
                        //     implementation-defined.
                        //
                        return kiss() * 4.656613e-10 - 1;
                }
        private:
                KISS kiss;
        };
        // --------------------------------------------------------------------

} }
#endif
