// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef NODE_HH_20130723
#define NODE_HH_20130723

#include <ostream>
#include <cstdint>
#include "real.hh"

namespace excyrender { namespace detail { namespace BIH {

   struct Node {
        uint32_t flags : 2;
        uint32_t index : 30;
        float clip[2];

        static Node Inner(tuple<real,real> const &clip, int axis, int index)
        {
            if (axis!=0 && axis!=1 && axis!=2)
                throw std::logic_error("Node::Inner: axis must be one of 0,1,2");
            Node ret;
            ret.clip[0] = get<0>(clip);
            ret.clip[1] = get<1>(clip);
            ret.flags = axis;
            ret.index = index;
            return ret;
        }

        static Node Leaf(int index)
        {
            Node ret;
            ret.flags = 3;
            ret.index = index;
            return ret;
        }

        static Node Bogus()
        {
            return Node();
        }

        bool empty() const { return std::fabs(clip[0]-clip[1])<=0; }
        bool leaf () const { return flags == 3; }

    private:
        Node() = default;
    };


    inline
    std::ostream& operator<< (std::ostream& os, Node const &n) {
        if (n.leaf()) {
            os << "leaf{" << n.index << "}";
        } else {
            os << "innr{[" << n.clip[0] << ".." << n.clip[1] << "], " << n.flags << "}";
        }
        return os;
    }

} } }

#endif
