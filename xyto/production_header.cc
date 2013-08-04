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

#include "pattern.hh"
#include "production_header.hh"

namespace xyto { 

std::string ProductionHeader::name() const {
        return name_;
}



void ProductionHeader::setName(std::string const &name) {
        name_ = name;
}



boost::optional<Parameter> ProductionHeader::condition () const {
        return condition_;
}



void ProductionHeader::setCondition (Parameter const &cond) {
        condition_ = cond;
}



Pattern ProductionHeader::leftContext() const {
        return leftContext_;
}



void ProductionHeader::setLeftContext(Pattern const & p) {
        leftContext_ = p;
}



Pattern ProductionHeader::rightContext() const {
        return rightContext_;
}



void ProductionHeader::setRightContext(Pattern const & p) {
        rightContext_ = p;
}



Pattern ProductionHeader::pattern() const {
        return pattern_;
}



void ProductionHeader::setPattern(Pattern const & p) {
        pattern_ = p;
}

}
