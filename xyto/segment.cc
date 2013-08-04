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

#include <iostream>

#include "segment.hh"
#include "pattern.hh"
#include "parameterlist.hh"

namespace xyto { 

bool operator == (Segment const &lhs, Segment const &rhs) {
        if (lhs.type() != rhs.type())
                return false;
        switch (lhs.type()) {
        case Segment::Letter:
                return lhs.name() == rhs.name()
                    && lhs.parameterList().size() == rhs.parameterList().size();
        case Segment::Branch:
                return lhs.branch() <= rhs.branch();
        }
}



bool operator != (Segment const &lhs, Segment const &rhs) {
        return !(lhs == rhs);
}


//--


Segment::Segment()
: branch_(new Pattern())
{
}



Segment::Segment (Segment const &rhs)
: name_(rhs.name_)
, parameterList_(rhs.parameterList_)
, branch_(new Pattern (*rhs.branch_))
, type_(rhs.type_)
{
}



Segment& Segment::operator= (Segment const &rhs) {
        name_ = rhs.name_;
        parameterList_ = rhs.parameterList_;
        *branch_ = *rhs.branch_;
        type_ = rhs.type_;
        return *this;
}



std::string Segment::name() const {
        return name_;
}


void Segment::setName(std::string const &name) {
        name_ = name;
}



ParameterList Segment::parameterList() const {
        if (type_ != Letter) {
                std::cerr <<
                 "'parameterList() const'"
                 " called on Segment which is a branch\n";
        }
        return parameterList_;
}



ParameterList &Segment::parameterList() {
        if (type_ != Letter) {
                std::cerr <<
                 "'parameterList()'"
                 " called on Segment which is a branch\n";
        }
        return parameterList_;
}



void Segment::setParameterList(ParameterList const &rhs) {
        if (type_ != Letter) {
                std::cerr <<
                 "'setParameterList(ParameterList const &rhs)'"
                 " called on Segment which is a branch\n";
        }
        parameterList_ = rhs;
}



Pattern Segment::branch() const {
        if (type_ != Branch) {
                std::cerr <<
                 "'branch() const'"
                 " called on Segment which is not a branch\n";
        }
        return *branch_;
}



Pattern& Segment::branch() {
        if (type_ != Branch) {
                std::cerr <<
                 "'branch()'"
                 " called on Segment which is not a branch\n";
        }
        return *branch_;
}



void Segment::setBranch(Pattern const &pat) {
        if (type_ != Branch) {
                std::cerr <<
                 "'setBranch()'"
                 " called on Segment which is not a branch\n";
        }
        branch() = pat;
}



void Segment::setType (Type type) {
        type_ = type;
}



Segment::Type Segment::type () const {
        return type_;
}

}
