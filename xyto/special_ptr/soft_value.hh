//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Copyright (C) 2010  Sebastian Mach (*1983)
// * mail: phresnel/at/gmail/dot/com
// * http://phresnel.org
// * http://picogen.org
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ifndef SOFT_VALUE_HH_INCLUDED_20100729
#define SOFT_VALUE_HH_INCLUDED_20100729

#include <boost/scoped_ptr.hpp>

namespace special_ptr {

template <typename T>
class soft_value {
public:
        soft_value () {}

        soft_value (soft_value const &rhs)
        : ptr(rhs.ptr ? new T(*rhs) : 0)
        {}

        soft_value (T const &rhs)
        : ptr(new T(rhs))
        {}

        soft_value& operator= (soft_value rhs) {
                swap (rhs);
                return *this;
        }

        void swap (soft_value &rhs) {
                ptr.swap (rhs.ptr);
        }

        operator bool () const { return (bool)ptr; }

        T & operator*() const { return *ptr; }
        T * operator->() const { return ptr.get(); }

private:
        boost::scoped_ptr<T> ptr;
};

template <typename T>
void swap (soft_value<T> &lhs, soft_value<T> &rhs) {
        lhs.swap (rhs);
}

}

#endif // SOFT_VALUE_HH_INCLUDED_20100729
