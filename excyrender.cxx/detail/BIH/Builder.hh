// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef BUILDER_HH_INCLUDED_20130723
#define BUILDER_HH_INCLUDED_20130723

#include "Data.hh"

namespace excyrender { namespace detail { namespace BIH {

    namespace detail {
        template <typename T>
        class Builder
        {
            typedef typename std::vector<T>::iterator iterator;
        public:
            void build(Data<T> &data, int max_depth)
            {
                data.aabb = exact_aabb(data.objects.begin(), data.objects.end());

                build_node(data.objects.begin(), data.objects.end(),
                           data.aabb,
                           max_depth,
                           data.nodes,
                           data.object_groups);
                data.nodes.shrink_to_fit();
            }

        private:

            static AABB exact_aabb(iterator it, iterator end) {
                Geometry::Point Max(-real_max, -real_max, -real_max),
                        Min(real_max, real_max, real_max);
                for ( ; it!=end; ++it) {
                    const auto bb = aabb(*it);
                    Max.x = max(Max.x, right(bb));
                    Max.y = max(Max.y, top(bb));
                    Max.z = max(Max.z, back(bb));
                    Min.x = min(Min.x, left(bb));
                    Min.y = min(Min.y, bottom(bb));
                    Min.z = min(Min.z, front(bb));
                }
                return {Min, Max};
            }

            /*static tuple<real,real> clip(iterator it, iterator end, int axis) {
                real Max = -real_max, Min = real_max;
                for ( ; it!=end; ++it) {
                    const auto bb = aabb(*it);
                    Max = max(Max, bb.max()[axis]);
                    Min = min(Min, bb.min()[axis]);
                }
                return make_tuple(Min, Max);
            }*/

            static real max_bound(iterator it, iterator end, int axis) {
                real Max = -real_max;
                for (; it!=end; ++it)
                    Max = max(Max, aabb(*it).max()[axis]);
                return Max;
            }

            static real min_bound(iterator it, iterator end, int axis) {
                real Min = real_max;
                for (; it!=end; ++it)
                    Min = min(Min, aabb(*it).min()[axis]);
                return Min;
            }

            static void build_node(const iterator first, const iterator last, AABB const &node_bb,
                                   int r,
                                   std::vector<Node> &nodes,
                                   std::vector<typename Data<T>::object_group> &groups)
            {
                using namespace Geometry;

                const int  axis = longest_axis(node_bb);
                //const auto clip_planes = clip(first, last, axis);

                if (std::distance(first, last) <= 5 || r<=0)
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
                            return center(aabb(obj))[axis] < split_plane; });

                    // Children bounding boxes, children, and current node finalization.
                    const auto children_bb = split(node_bb, axis);
                    build_node(first, pivot, get<0>(children_bb), r-1, nodes, groups);

                    const auto left  = max_bound(first, pivot, axis),
                               right = min_bound(pivot, last, axis);
                    nodes[where_our_node_at] = Node::Inner(make_tuple(left, right), axis,
                                                           nodes.size()-where_our_node_at);
                    build_node(pivot, last,  get<1>(children_bb), r-1, nodes, groups);
                }
            }
        };
    }

    template <typename T>
    void build (Data<T> &data, int max_rec) {
        detail::Builder<T>().build(data, max_rec);
    }

} } }

#endif
