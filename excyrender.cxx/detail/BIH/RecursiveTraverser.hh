// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef RECURSIVE_TRAVERSER_HH_INCLUDED_20130723
#define RECURSIVE_TRAVERSER_HH_INCLUDED_20130723

#include "Node.hh"
#include "Data.hh"
#include "Geometry/Ray.hh"

namespace excyrender { namespace detail { namespace BIH {

    namespace detail {
        template <typename T>
        struct RecursiveTraverserTraits {
            using intersection_type = decltype(intersect(*((T*)nullptr),
                                               *((Geometry::Ray*)nullptr)));

            // "Import" some global functions to circumvent ADL-issues within RecursiveTraverser.
            // Note how intersect has a suffix here.
            static intersection_type intersect_(T const &o, Geometry::Ray const &ray) noexcept {
                return intersect(o, ray);
            }
        };
    }

    template <typename T>
    class RecursiveTraverser {
        const Data<T> &data;

    public:
        typedef typename detail::RecursiveTraverserTraits<T>::intersection_type intersection_type;

        RecursiveTraverser(Data<T> const &data) : data(data) {}

        intersection_type intersect(Geometry::Ray const &ray) const noexcept
        {
            const auto initial = excyrender::intersect(data.aabb, ray);
            if (!initial)
                return intersection_type();
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
        intersection_type
         traverse_rec(Node const* node, Geometry::Ray const &ray, real A, real B) const noexcept
        {
            if (A >= B) {
                return intersection_type();
            }

            if (node->leaf())
            {
                intersection_type nearest;
                typename Data<T>::object_group g = data.object_groups[node->index()];
                for (auto it=get<0>(g), end=get<1>(g); it!=end; ++it) {
                    if (auto tmp = detail::RecursiveTraverserTraits<T>::intersect_(*it, ray)) {
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
            return intersection_type();
        }
    };


    template <typename T>
    inline
    typename RecursiveTraverser<T>::intersection_type
      recursive_intersect(Data<T> const &data, Geometry::Ray const &ray) noexcept
    {
        return RecursiveTraverser<T>(data).intersect(ray);
    }

    template <typename T>
    inline
    bool recursive_occludes(Data<T> const &data, Geometry::Point const &a, Geometry::Point const &b) noexcept
    {
        return RecursiveTraverser<T>(data).occludes(a, b);
    }

    template <typename T>
    inline
    bool recursive_occludes(Data<T> const &data, Geometry::Point const &a, Geometry::Direction const &b) noexcept
    {
        return RecursiveTraverser<T>(data).occludes(a, b);
    }


} } }

#endif // RECURSIVE_TRAVERSER_HH_INCLUDED_20130723
