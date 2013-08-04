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

#ifndef SYMBOL_HH_INCLUDED_20100726
#define SYMBOL_HH_INCLUDED_20100726

#include <string>
#include <boost/shared_ptr.hpp>
#include "parameterlist.hh"

namespace xyto { 

class ParameterList;
class Pattern;

class Segment {
public:
        enum Type {
                Letter,
                Branch
        };

        Segment();
        Segment (Segment const &rhs);
        Segment& operator= (Segment const &rhs);

        std::string name() const;
        void setName(std::string const &name);

        void setType (Type type) ;
        Type type () const;

        ParameterList parameterList() const;
        ParameterList &parameterList();
        void setParameterList(ParameterList const &rhs);

        Pattern branch () const ;
        Pattern &branch ();
        void setBranch(Pattern const &pat);
private:
        std::string name_;
        ParameterList parameterList_;
        boost::shared_ptr<Pattern> branch_;
        Type type_;
};

bool operator == (Segment const &lhs, Segment const &rhs);
bool operator != (Segment const &lhs, Segment const &rhs);

}

#endif // SYMBOL_HH_INCLUDED_20100726
