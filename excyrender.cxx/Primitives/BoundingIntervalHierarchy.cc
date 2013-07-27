// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "detail/BIH/RecursiveTraverser.hh"
#include "detail/BIH/Builder.hh"
#include "BoundingIntervalHierarchy.hh"

using namespace excyrender::Geometry;

namespace excyrender { namespace Primitives {


// For use in detail::BIH.
inline optional<Intersection> intersect (std::shared_ptr<FinitePrimitive> const &fp, Ray const &ray) {
    return fp->intersect(ray);
}
inline AABB aabb (std::shared_ptr<FinitePrimitive> const &fp) {
    return fp->aabb();
}



optional<Intersection> BoundingIntervalHierarchy::intersect(Ray const &ray) const noexcept {
    return detail::BIH::recursive_intersect(data_, ray);
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



std::shared_ptr<BoundingIntervalHierarchy> BoundingIntervalHierarchyBuilder::finalize() {
    if (finalized) {
        throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'finalize()' "
                               "but builder is finalized already");
    }
    finalized = true;

    build(data_);

    std::shared_ptr<BoundingIntervalHierarchy> ret (new BoundingIntervalHierarchy);
    ret->data_ = std::move(data_);
    return ret;
}

void BoundingIntervalHierarchyBuilder::add (std::initializer_list<std::shared_ptr<Primitives::FinitePrimitive>> prims) {
    if (finalized) {
        throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'add({...})' "
                               "but builder is finalized already");
    }
    for (auto p : prims)
        data_.objects.push_back(p);
}

void BoundingIntervalHierarchyBuilder::add (std::shared_ptr<Primitives::FinitePrimitive> prim) {
    if (finalized) {
        throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'add()' "
                               "but builder is finalized already");
    }
    data_.objects.push_back(prim);
}


} }