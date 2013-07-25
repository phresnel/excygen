// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef NODE_HH_20130723
#define NODE_HH_20130723

#include <ostream>
#include <cstdint>
#include <stdexcept>
#include "real.hh"

namespace excyrender { namespace detail { namespace BIH {

    class Node {
    public:

        static Node Inner(tuple<real,real> const &clip, int axis, int index) noexcept
        {
            if (axis!=0 && axis!=1 && axis!=2)
                throw std::logic_error("Node::Inner: axis must be one of 0,1,2");
            Node ret;
            ret.clip_[0] = get<0>(clip);
            ret.clip_[1] = get<1>(clip);
            ret.flags_ = axis;
            ret.index_ = index;
            return ret;
        }

        static Node Leaf(int index) noexcept
        {
            Node ret;
            ret.flags_ = 3;
            ret.index_ = index;
            return ret;
        }

        constexpr static Node Bogus() noexcept
        {
            return Node();
        }

        constexpr bool empty() noexcept {
            return std::fabs(clip_[0]-clip_[1])<=0;
        }

        constexpr bool leaf () noexcept {
            return flags_ == 3;
        }

        constexpr int axis() noexcept {
            return flags_;
        }

        constexpr int index() noexcept {
            return index_;
        }

        constexpr real left() noexcept {
            return clip_[0];
        }

        constexpr real right() noexcept {
            return clip_[1];
        }

    private:
        Node() = default;

        uint32_t flags_ : 2;
        uint32_t index_ : 30;
        float clip_[2];
    };

} } }

#endif
