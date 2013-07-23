// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef BOUNDINGINTERVALHIERARCHY_HH_INCLUDED_20130721
#define BOUNDINGINTERVALHIERARCHY_HH_INCLUDED_20130721

#include "Primitives/FinitePrimitive.hh"
#include "Shapes/FiniteShape.hh"
#include "Geometry/Direction.hh"


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
    class Impl;
    std::shared_ptr<Impl> impl_;
};



class BoundingIntervalHierarchyBuilder {
public:
    class Group {
    public:
        Group()                         = delete;
        Group(Group const&)             = delete;
        Group& operator=(Group const &) = delete;

        void add (std::shared_ptr<Primitives::FinitePrimitive> shape) {
            if (*finalized) {
                throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'add()' "
                                       "but builder is finalized already");
            }
        }
    private:
        Group(BoundingIntervalHierarchyBuilder &builder)
            : finalized(builder.finalized)
        {
        }

        std::shared_ptr<bool> finalized;

        friend class BoundingIntervalHierarchyBuilder;
    };
    friend class Group;

    BoundingIntervalHierarchyBuilder() = default;
    BoundingIntervalHierarchyBuilder(BoundingIntervalHierarchyBuilder const &)            = delete;
    BoundingIntervalHierarchyBuilder& operator=(BoundingIntervalHierarchyBuilder const &) = delete;

    std::shared_ptr<Group> group(Photometry::Surface::BSDF bsdf) {
        if (*finalized) {
                throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'group()' "
                                       "but builder is finalized already");
        }
        return std::shared_ptr<Group>{new Group(*this)};
    }

    std::shared_ptr<BoundingIntervalHierarchy> finalize() {
        if (*finalized) {
                throw std::logic_error("BoundingIntervalHierarchyBuilder::Group: called 'finalize()' "
                                       "but builder is finalized already");
        }
        *finalized = true;
        return std::shared_ptr<BoundingIntervalHierarchy>{new BoundingIntervalHierarchy()};
    }

private:
    std::shared_ptr<bool> finalized = std::shared_ptr<bool>(new bool(false));
};


// TODO: once functional, add a specialized BIH for any type T <- FiniteShape

} }

#endif // BOUNDINGINTERVALHIERARCHY_HH_INCLUDED_20130721
