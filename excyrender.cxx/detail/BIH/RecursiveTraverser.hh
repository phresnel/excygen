// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef RECURSIVE_TRAVERSER_HH_INCLUDED_20130723
#define RECURSIVE_TRAVERSER_HH_INCLUDED_20130723

#include "Node.hh"
#include "Geometry/Ray.hh"

namespace excyrender { namespace detail { namespace BIH {

    template <typename T, typename BaseClass>
    class RecursiveTraverser : public BaseClass {
    public:
        RecursiveTraverser(Data<T> &data) : data(data) {}

        auto intersect(Geometry::Ray const &ray) const noexcept
        -> decltype(((T*)(nullptr))->intersect(ray))
        {
            using RetT = decltype(((T*)(nullptr))->intersect(ray));
            const auto initial = excyrender::intersect(data.aabb, ray);
            if (!initial)
                return RetT();
            const real A = max(real(0),get<0>(*initial)),
                       B = get<1>(*initial);
            return traverse_rec(&data.nodes[0], ray, A, B);
        }

        bool occludes(Geometry::Point const &a, Geometry::Point const &b) const noexcept
        {
            return !!intersect(Geometry::Ray(a, Geometry::Direction::Normalize(b-a)));
        }

        bool occludes(Geometry::Point const &a, Geometry::Direction const &b) const noexcept
        {
            return !!intersect(Geometry::Ray(a, b));
        }

    private:
        Data<T> &data;

    private:
        auto traverse_rec(Node const* node, Geometry::Ray const &ray, real A, real B) const
        -> decltype(((T*)(nullptr))->intersect(ray))
        {
            using RetT = decltype(((T*)(nullptr))->intersect(ray));

            if (A >= B) {
                return RetT();
            }

            if (node->leaf())
            {
                RetT nearest;
                typename Data<T>::object_group g = data.object_groups[node->index()];
                for (auto it=get<0>(g), end=get<1>(g); it!=end; ++it) {
                    if (auto tmp = it->intersect(ray)) {
                        const auto t = distance(*tmp);
                        if (t<A || t>B)
                            continue;
                        if (!nearest || t < distance(*nearest)) {
                            nearest = tmp;
                        }
                    }
                }
                return nearest;
            }
            else
            {
                const int axis = node->axis();
                const real t1 = (node->left()  - ray.origin[axis]) / ray.direction[axis];
                const real t2 = (node->right() - ray.origin[axis]) / ray.direction[axis];

                if (ray.direction[axis] < 0) {
                    auto a = traverse_rec(node+1, ray, A, min(t1,B));
                    if (a) B = min(B, distance(*a));
                    auto b = traverse_rec(node+node->index(), ray, max(t2,A), B);

                    if (b) return b;
                    return a;
                } else {
                    auto a = traverse_rec(node+node->index(), ray, A, min(t2,B));
                    if (a) B = min(B, distance(*a));
                    auto b = traverse_rec(node+1, ray, max(t1,A), B);

                    if (b) return b;
                    return a;
                }
            }
            return RetT();
        }
    };

} } }

#endif // RECURSIVE_TRAVERSER_HH_INCLUDED_20130723
