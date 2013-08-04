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
#include "production_body.hh"

namespace xyto { 

Pattern ProductionBody::pattern() const {
        return pattern_;
}



void ProductionBody::setPattern(Pattern const & p) {
        pattern_ = p;
}



double ProductionBody::probability() const {
        return probability_;
}



void ProductionBody::setProbability(double probability) {
        probability_ = probability;
}



ProductionBody::ProductionBody() {
}



ProductionBody::ProductionBody(ProductionBody const &rhs)
: pattern_(rhs.pattern_), probability_(rhs.probability_)
{
}



ProductionBody& ProductionBody::operator= (ProductionBody const &rhs) {
        pattern_ = rhs.pattern_;
        probability_ = rhs.probability_;
        return *this;
}

}

