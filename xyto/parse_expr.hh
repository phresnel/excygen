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

#ifndef PARSE_EXPR_HH_INCLUDED_20100730
#define PARSE_EXPR_HH_INCLUDED_20100730

#include <boost/optional.hpp>
#include "parameter.hh"
#include "token.hh"

namespace xyto { 

boost::optional<Parameter> parse_term (TokenIterator it, TokenIterator end,
                                       TokenIterator &behind);
boost::optional<Parameter> parse_expr (TokenIterator it, TokenIterator end,
                                       TokenIterator &behind);
boost::optional<Parameter> parse_rel(TokenIterator it, TokenIterator end,
                                     TokenIterator &behind);
boost::optional<Parameter> parse_logical(TokenIterator it, TokenIterator end,
                                     TokenIterator &behind);
}

#endif // PARSE_EXPR_HH_INCLUDED_20100730
