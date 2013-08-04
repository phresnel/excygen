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

#include "xyto_ios.hh"
#include "token.hh"
#include "pattern.hh"
#include "parameter.hh"
#include "parameterlist.hh"
#include "segment.hh"
#include "production.hh"
#include "lsystem.hh"

#include "turtlevector.hh"

namespace xyto {

std::ostream& operator<< (std::ostream& o, Token const &rhs) {
        o << "[" << rhs.value() << "]";
        return o;
}

std::ostream& operator<< (std::ostream& o, TokenVector const &rhs) {
        for (TokenVector::const_iterator it=rhs.begin();
             it != rhs.end();
             ++it
        ) {
                if (it != rhs.begin()) {
                        o << " ";
                }
                o << *it;
        }
        return o;
}

std::ostream& operator<< (std::ostream& o, Parameter const& rhs) {
        switch (rhs.type()) {
        case Parameter::Identifier:
                o << rhs.identifier() /*<< ":id"*/;
                break;
        case Parameter::Integer:
                o << rhs.integer() /*<< ":int"*/;
                break;
        case Parameter::Real:
                o << rhs.real()  /*<< ":real"*/;
                break;
        case Parameter::ParameterIndex:
                o << "#" << rhs.parameterIndex();
                break;
        /*case Parameter::Constant:
                o << rhs.constant().name();
                break;*/
        case Parameter::Negate:
                o << "-" << rhs.unaryParameter();
                break;
        case Parameter::Addition:        o << "(+ " << rhs.lhs() << " "
                                                    << rhs.rhs() << ")";
                                         break;
        case Parameter::Subtraction:     o << "(- " << rhs.lhs() << " "
                                                    << rhs.rhs() << ")";
                                         break;
        case Parameter::Multiplication:  o << "(* " << rhs.lhs() << " "
                                                    << rhs.rhs() << ")";
                                         break;
        case Parameter::Division:        o << "(/ " << rhs.lhs() << " "
                                                    << rhs.rhs() << ")";
                                         break;
        case Parameter::LessThan:        o << "(< " << rhs.lhs() << " "
                                                    << rhs.rhs() << ")";
                                         break;
        case Parameter::LessEqual:       o << "(<= " << rhs.lhs() << " "
                                                    << rhs.rhs() << ")";
                                         break;
        case Parameter::GreaterThan:     o << "(> " << rhs.lhs() << " "
                                                    << rhs.rhs() << ")";
                                         break;
        case Parameter::GreaterEqual:    o << "(>= " << rhs.lhs() << " "
                                                     << rhs.rhs() << ")";
                                         break;

        case Parameter::LogicalAnd:      o << "(&& " << rhs.lhs() << " "
                                                     << rhs.rhs() << ")";
                                         break;
        case Parameter::LogicalOr:       o << "(|| " << rhs.lhs() << " "
                                                     << rhs.rhs() << ")";
                                         break;
        case Parameter::LogicalXor:      o << "(xor " << rhs.lhs() << " "
                                                      << rhs.rhs() << ")";
                                         break;
        }
        return o;
}

std::ostream& operator<< (std::ostream& o, ParameterList const& rhs) {
        if (rhs.size()) {
                o << "(";
                for (unsigned int i=0; i<rhs.size(); ++i) {
                        if (i) o << ", ";
                        o << rhs[i];
                }
                o << ")";
        }
        return o;
}


std::ostream& operator<< (std::ostream& o, Segment const& rhs) {
        switch (rhs.type()) {
        case Segment::Letter:
                o << rhs.name();
                o << rhs.parameterList();
                break;
        case Segment::Branch:
                o << "[" << rhs.branch() << "]";
                break;
        }
        return o;
}

std::ostream& operator<< (std::ostream& o, Pattern const& rhs) {
        if (rhs.size()) {
                o << rhs[0];
        }
        for (unsigned int i=1; i<rhs.size(); ++i) {
                o << " " << rhs[i];
        }
        return o;
}

std::ostream& operator<< (std::ostream& o, ProductionHeader const& rhs){
        o << rhs.name() << ": ";

        if (!rhs.leftContext().empty())
                o << rhs.leftContext() << " < ";

        o << rhs.pattern();

        if (!rhs.rightContext().empty())
                o << " > " << rhs.rightContext();

        if (rhs.condition())
                o << " : if (" << *rhs.condition() << ") ";
        return o;
}

std::ostream& operator<< (std::ostream& o, ProductionBody const& rhs) {
        o << rhs.pattern();
        return o;
}

std::ostream& operator<< (std::ostream& o,
                                 std::vector<ProductionBody> const& rhs) {
        if (rhs.size() == 1) {
                o << rhs[0].pattern();
        } else if (rhs.size() > 1) {
                o << "\n";
                for (unsigned int i=0; i<rhs.size(); ++i) {
                        o << "        "
                          << "(" << rhs[i].probability() << ") "
                          << rhs[i].pattern() << "\n";
                }
        }
        return o;
}

std::ostream& operator<< (std::ostream& o, Production const& rhs) {
        o << rhs.header() << " --> " << rhs.bodies() << ";";
        return o;
}

std::ostream& operator<< (std::ostream &o,
                          std::vector<Production> const &prods)
{
        for (unsigned int i=0; i<prods.size(); ++i) {
                o << prods[i] << '\n';
        }
        return o;
}

std::ostream& operator<< (std::ostream &o, LSystem const &ls) {
        const std::vector<Constant> consts = ls.constants();
        for (unsigned int u=0; u<consts.size(); ++u) {
                o << consts[u].name() << " = " << consts[u].toReal() << ";\n";
        }

        o << "axiom: " << ls.axiom() << '\n';
        o << ls.productions();

        return o;
}




std::ostream& operator<< (std::ostream& o, TurtleVector const &rhs) {
        o << "[" << rhs.x << ", " << rhs.y << ", " << rhs.z << "]";
        return o;
}

}
