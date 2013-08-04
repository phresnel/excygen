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

#ifndef PRODUCTION_HH_INCLUDED_20100726
#define PRODUCTION_HH_INCLUDED_20100726

#include "production_header.hh"
#include "production_body.hh"
#include <vector>

namespace portable_rng { namespace marsaglia {
        class UNI;
} }

namespace xyto {

class Production {
public:
        Production();
        Production(Production const &rhs);
        Production& operator= (Production const &rhs);

        ProductionHeader header() const;
        void setHeader(ProductionHeader const &h);

        /*ProductionBody body() const;
        void setBody(ProductionBody const &h);*/
        std::vector<ProductionBody> bodies () const;
        std::vector<ProductionBody>& bodies ();
        void setBodies(std::vector<ProductionBody> const &);
        ProductionBody pickBody(portable_rng::marsaglia::UNI &) const;
private:
        ProductionHeader header_;
        std::vector<ProductionBody> bodies_;
};

}

#endif // PRODUCTION_HH_INCLUDED_20100726
