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

#include "token.hh"

#include <sstream>

namespace xyto { 

Token::Token (Type type_,
       const CodeIterator &from,
       const CodeIterator &to
)
: type_(type_)
, from_(from)
, to_(to)
, value_("")
{
        for (CodeIterator curr=from_; curr!=to_; ++curr) {
                value_ += *curr;
        }
}



Token::Token (Type type_,
        const CodeIterator &from,
        const CodeIterator &to,
        const std::string &value_
)
: type_(type_)
, from_(from)
, to_(to)
, value_(value_)
{}



Token::Token (Token const &rhs)
: type_(rhs.type_)
, from_(rhs.from_)
, to_(rhs.to_ )
, value_(rhs.value_)
{}



Token& Token::operator= (Token const &rhs) {
        type_ = rhs.type_;
        from_ = rhs.from_;
        to_ = rhs.to_;
        value_ = rhs.value_;
        return *this;
}



Token::Type Token::type() const {
        return type_;
}



std::string Token::value() const {
        return value_;
}



double Token::valueAsReal () const {
        std::stringstream ss;
        ss << value_;
        double ret;
        ss >> ret;
        return ret;
}



int Token::valueAsInteger () const {
        std::stringstream ss;
        ss << value_;
        int ret;
        ss >> ret;
        return ret;
}



CodeIterator Token::from() const {
        return from_;
}



CodeIterator Token::to() const {
        return to_;
}

}
