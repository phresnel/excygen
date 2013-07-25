// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef DATA_HH_20130723
#define DATA_HH_20130723

#include "AABB.hh"
#include "Node.hh"
#include <vector>

namespace excyrender { namespace detail { namespace BIH {

    template <typename T>
    struct Data {
        std::vector<T> objects;

        AABB aabb = AABB(Geometry::Point(0,0,0),Geometry::Point(1,1,1)); // we could skip this meaningless initialization by separating builder, data and traverser
        std::vector<Node> nodes;

        typedef typename std::vector<T>::const_iterator object_iterator;
        typedef tuple<object_iterator, object_iterator> object_group;
        std::vector<object_group> object_groups;

        // below decltype can't be used
        //using intersection_type = decltype(intersect(*((T*)(nullptr)), *(Geometry::Ray*(nullptr))));
    };

} } }

#endif
