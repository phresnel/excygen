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

#include "parameterlist.hh"

namespace xyto { 

unsigned int ParameterList::size() const {
        return parameters_.size();
}



void ParameterList::push_back(Parameter const &sym) {
        parameters_.push_back(sym);
}



bool ParameterList::empty() const {
        return size() == 0;
}



Parameter& ParameterList::operator [] (unsigned int i) {
        return parameters_[i];
}



const Parameter& ParameterList::operator [] (unsigned int i) const {
        return parameters_[i];
}



ParameterList::ParameterList() {
}



ParameterList::ParameterList(ParameterList const &rhs)
: parameters_(rhs.parameters_)
{
}



ParameterList& ParameterList::operator= (ParameterList const &rhs) {
        parameters_ = rhs.parameters_;
        return *this;
}

}

