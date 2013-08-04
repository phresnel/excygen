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

#include "production.hh"
#include "portable_rng/kiss.hh"
#include <iostream>

namespace xyto {

ProductionHeader Production::header() const {
                return header_;
}



void Production::setHeader(ProductionHeader const &h) {
        header_ = h;
}



std::vector<ProductionBody> Production::bodies() const {
        return bodies_;
}



std::vector<ProductionBody> & Production::bodies() {
        return bodies_;
}


ProductionBody Production::pickBody(
        portable_rng::marsaglia::UNI &rng
) const {
        if (bodies_.size() == 1)
                return bodies_[0];

        double p = rng();
        int i = -1;
        do {
                ++i;
                p-=bodies_[i].probability();
        } while (p>0);
        return bodies_[i];
}



void Production::setBodies(std::vector<ProductionBody> const &h) {
        bodies_ = h;
}



Production::Production() {
}



Production::Production(Production const &rhs)
: header_(rhs.header_)
, bodies_(rhs.bodies_)
{
}



Production& Production::operator= (Production const &rhs) {
        header_ = rhs.header_;
        bodies_ = rhs.bodies_;
        return *this;
}

}
