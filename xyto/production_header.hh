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

#ifndef PRODUCTION_HEADER_HH_INCLUDED_20100726
#define PRODUCTION_HEADER_HH_INCLUDED_20100726

#include <string>
#include <boost/optional.hpp>
#include "pattern.hh"

namespace xyto { 

class ProductionHeader {
public:
        std::string name() const;
        void setName(std::string const &name);

        Pattern leftContext()  const;
        Pattern rightContext() const;
        Pattern pattern() const;

        boost::optional<Parameter> condition () const ;
        void setCondition (Parameter const &cond);

        void setLeftContext(Pattern const & p);
        void setRightContext(Pattern const & p);
        void setPattern(Pattern const & p);

private:
        std::string name_;
        Pattern leftContext_,
                rightContext_,
                pattern_;
        boost::optional<Parameter> condition_;
};

}

#endif // PRODUCTION_HEADER_HH_INCLUDED_20100726
