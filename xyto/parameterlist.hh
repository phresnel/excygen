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

#ifndef PARAMETERLIST_HH_INCLUDED_20100726
#define PARAMETERLIST_HH_INCLUDED_20100726

#include <vector>
#include "parameter.hh"

namespace xyto { 

class Parameter;

class ParameterList {
public:
        ParameterList();
        ParameterList(ParameterList const &rhs);
        ParameterList& operator= (ParameterList const &rhs);

        unsigned int size() const;
        void push_back(Parameter const &sym);
        bool empty() const;

        Parameter &operator [] (unsigned int i);
        const Parameter &operator [] (unsigned int i) const;
private:
        std::vector<Parameter> parameters_;
};

}

#endif // PARAMETERLIST_HH_INCLUDED_20100726
