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

#ifndef XYTO_IOS_HH_INCLUDED_20100726
#define XYTO_IOS_HH_INCLUDED_20100726

#include <vector>
#include <fstream>

namespace xyto {

class Token;
typedef std::vector<Token> TokenVector;
class Pattern;
class Parameter;
class ParameterList;
class Segment;
class ProductionHeader;
class ProductionBody;
class ProductionBody;
class Production;
class LSystem;

class TurtleVector;


std::ostream& operator<< (std::ostream& o, TurtleVector const &rhs);

std::ostream& operator<< (std::ostream&, LSystem const&);
std::ostream& operator<< (std::ostream&, Token const&);
std::ostream& operator<< (std::ostream&, TokenVector const&);
std::ostream& operator<< (std::ostream&, Pattern const&);
std::ostream& operator<< (std::ostream&, Parameter const&);
std::ostream& operator<< (std::ostream&, ParameterList const&);
std::ostream& operator<< (std::ostream&, Segment const&);
std::ostream& operator<< (std::ostream&, Pattern const&);
std::ostream& operator<< (std::ostream&, ProductionHeader const&);
std::ostream& operator<< (std::ostream&, ProductionBody const&);
std::ostream& operator<< (std::ostream&, std::vector<ProductionBody>const&);
std::ostream& operator<< (std::ostream&, Production const&);
std::ostream& operator<< (std::ostream&, std::vector<Production>const&);

}

#endif // XYTO_IOS_HH_INCLUDED_20100726
