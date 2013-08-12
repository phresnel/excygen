// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "detail/BIH/RecursiveTraverser.hh"
#include "detail/BIH/Builder.hh"
#include "Shapes/BoundingIntervalHierarchy.hh"
#include "DebugPixel.hh"

using namespace excyrender::Geometry;

namespace excyrender { namespace Shapes {


// For use in detail::BIH.
inline optional<DifferentialGeometry> intersect (std::shared_ptr<FiniteShape> const &fp, Ray const &ray) {
    return fp->intersect(ray);
}
inline AABB aabb (std::shared_ptr<FiniteShape> const &fp) {
    return fp->aabb();
}



optional<DifferentialGeometry> BoundingIntervalHierarchy::intersect(Ray const &ray) const noexcept {
    int steps = 0;
    auto ret = detail::BIH::recursive_intersect(data_, ray, steps);
    if (current_debug)
        current_debug->traversal0 += steps;
    return ret;
}

bool BoundingIntervalHierarchy::occludes(Point const &a, Point const &b) const noexcept {
    return detail::BIH::recursive_occludes(data_, a, b);
}

bool BoundingIntervalHierarchy::occludes(Point const &a, Direction const &b) const noexcept {
    return detail::BIH::recursive_occludes(data_, a, b);
}

AABB BoundingIntervalHierarchy::aabb() const noexcept {
    return data_.aabb;
}



std::shared_ptr<BoundingIntervalHierarchy> BoundingIntervalHierarchyBuilder::finalize(int max_rec) {
    if (finalized) {
        throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'finalize()' "
                               "but builder is finalized already");
    }
    finalized = true;

    build(data_, max_rec);

    std::shared_ptr<BoundingIntervalHierarchy> ret (new BoundingIntervalHierarchy);
    ret->data_ = std::move(data_);
    return ret;
}

void BoundingIntervalHierarchyBuilder::add (std::initializer_list<std::shared_ptr<Shapes::FiniteShape>> prims) {
    if (finalized) {
        throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'add({...})' "
                               "but builder is finalized already");
    }
    for (auto p : prims)
        data_.objects.push_back(p);
}

void BoundingIntervalHierarchyBuilder::add (std::shared_ptr<Shapes::FiniteShape> prim) {
    if (finalized) {
        throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'add()' "
                               "but builder is finalized already");
    }
    data_.objects.push_back(prim);
}


} }
