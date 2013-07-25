// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef BOUNDINGINTERVALHIERARCHY_HH_INCLUDED_20130721
#define BOUNDINGINTERVALHIERARCHY_HH_INCLUDED_20130721

#include "Primitives/FinitePrimitive.hh"
#include "Geometry/Direction.hh"
#include "detail/BIH/Data.hh"
#include <memory>
#include <initializer_list>


namespace excyrender { namespace Primitives {

class BoundingIntervalHierarchy final : public FinitePrimitive {
    public:
        BoundingIntervalHierarchy(BoundingIntervalHierarchy const &)            = delete;
        BoundingIntervalHierarchy& operator=(BoundingIntervalHierarchy const &) = delete;

        optional<Intersection> intersect(Geometry::Ray const &) const noexcept;
        bool occludes(Geometry::Point const &a, Geometry::Point const &b) const noexcept;
        bool occludes(Geometry::Point const &a, Geometry::Direction const &b) const noexcept;
        AABB aabb() const noexcept;

    private:
        friend class BoundingIntervalHierarchyBuilder;

        BoundingIntervalHierarchy() = default;
        detail::BIH::Data<std::shared_ptr<FinitePrimitive>> data_;
    };

} }


namespace excyrender { namespace Primitives {
    class BoundingIntervalHierarchyBuilder {
    public:
        BoundingIntervalHierarchyBuilder() = default;
        BoundingIntervalHierarchyBuilder(BoundingIntervalHierarchyBuilder const &)            = delete;
        BoundingIntervalHierarchyBuilder& operator=(BoundingIntervalHierarchyBuilder const &) = delete;

        void add (std::initializer_list<std::shared_ptr<Primitives::FinitePrimitive>> prims);
        void add (std::shared_ptr<Primitives::FinitePrimitive> prim);
        std::shared_ptr<BoundingIntervalHierarchy> finalize();

    private:
        bool finalized = false;
        detail::BIH::Data<std::shared_ptr<FinitePrimitive>> data_;
    };

} }

#endif // BOUNDINGINTERVALHIERARCHY_HH_INCLUDED_20130721
