// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef BUILDER_HH_INCLUDED_20130723
#define BUILDER_HH_INCLUDED_20130723

#include "Data.hh"

namespace excyrender { namespace detail { namespace BIH {

    template <typename T>
    class Builder
    {
        typedef typename std::vector<T>::iterator iterator;
    public:
        void start_build(Data<T> &data, int recursions_left = 10)
        {
            data.aabb = exact_aabb(data.objects.begin(), data.objects.end());

            build_node(data.objects.begin(), data.objects.end(),
                       data.aabb,
                       0,
                       data.nodes,
                       data.object_groups);
            data.nodes.shrink_to_fit();
        }

    private:

        static AABB exact_aabb(iterator it, iterator end) {
            Geometry::Point Max(-real_max, -real_max, -real_max),
                            Min(real_max, real_max, real_max);
            for ( ; it!=end; ++it) {
                const auto bb = it->aabb();
                Max.x = max(Max.x, right(bb));
                Max.y = max(Max.y, top(bb));
                Max.z = max(Max.z, back(bb));
                Min.x = min(Min.x, left(bb));
                Min.y = min(Min.y, bottom(bb));
                Min.z = min(Min.z, front(bb));
            }
            return {Min, Max};
        }

        static tuple<real,real> clip(iterator it, iterator end, int axis) {
            real Max = -real_max, Min = real_max;
            for ( ; it!=end; ++it) {
                const auto bb = it->aabb();
                Max = max(Max, bb.max()[axis]);
                Min = min(Min, bb.min()[axis]);
            }
            return make_tuple(Min, Max);
        }

        static void build_node(const iterator first, const iterator last, AABB const &node_bb,
                               int r,
                               std::vector<Node> &nodes,
                               std::vector<typename Data<T>::object_group> &groups)
        {
            using namespace Geometry;

            const int  axis = longest_axis(node_bb);
            const auto clip_planes = clip(first, last, axis);

            if (std::distance(first, last) <= 1 || r>5)
            {
                groups.emplace_back(first, last);
                nodes.push_back(Node::Leaf(groups.size()-1));
                return;
            }
            else
            {
                // Reserve space.
                nodes.push_back (Node::Bogus());
                const auto where_our_node_at = nodes.size()-1;

                // Find pivot object.
                const real split_plane = center(node_bb, axis);
                const auto pivot = std::partition (first, last, [&](T const &obj) {
                                                 return center(obj.aabb())[axis] < split_plane; });

                // Children bounding boxes, children, and current node finalization.
                const auto children_bb = split(node_bb, axis);
                build_node(first, pivot, get<0>(children_bb), r+1, nodes, groups);
                nodes[where_our_node_at] = Node::Inner(clip_planes, axis,
                                                           nodes.size()-where_our_node_at);
                build_node(pivot, last,  get<1>(children_bb), r+1, nodes, groups);
            }
        }

        static void debug (std::ostream &os, Node const *node, int deep=0) {
            for (int i=0; i<deep*4; ++i)
                os << ' ';
            os << *node << '\n';
            if (node->leaf())
                return;
            debug(os, node+1, deep+1);
            debug(os, node+node->index, deep+1);
        }

    };

} } }

#endif
