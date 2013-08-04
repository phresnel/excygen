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

//#include "ignore_strict_aliasing" // because of boost::optional
#include "parse_expr.hh"
#include <stdexcept>
#include "xyto_ios.hh"

namespace xyto { 
namespace {

boost::optional<Parameter> parse_factor (TokenIterator it,
                   TokenIterator end,
                   TokenIterator &behind
) {
        Parameter ret;
        switch (it->type()) {
        case Token::Integer:
                ret.setType(Parameter::Integer);
                ret.setInteger(it->valueAsInteger());
                behind = ++it;
                return ret;
        case Token::Real:
                ret.setType(Parameter::Real);
                ret.setReal(it->valueAsReal());
                behind = ++it;
                return ret;
        case Token::Identifier:
                ret.setType(Parameter::Identifier);
                ret.setIdentifier(it->value());
                behind = ++it;
                return ret;
        case Token::Minus: {
                ret.setType(Parameter::Negate);
                ++it;
                if (it == end)
                        return boost::optional<Parameter>();
                const boost::optional<Parameter> p =
                                        parse_factor(it, end, behind);
                it = behind;
                if (!p)
                        return boost::optional<Parameter>();

                // I have implemented full constant propagation already
                // for the next quatsch revision, but I must get this L-System
                // thing ready ASAP.
                // This is really only the most basic form of it so that an
                // axiom may look like "foo(-0.5)".
                if (p->type() == Parameter::Integer) {
                        Parameter ret;
                        ret.setType(Parameter::Integer);
                        ret.setInteger (-p->integer());
                        return ret;
                } else if (p->type() == Parameter::Real) {
                        Parameter ret;
                        ret.setType(Parameter::Real);
                        ret.setReal (-p->real());
                        return ret;
                }
                ret.setUnaryParameter(*p);
                return ret;
        } break;
        default:
                break;
        };
        return boost::optional<Parameter>();;
}


Parameter::Type tokenTypeToParameterType (Token::Type tok) {
        switch (tok) {
        case Token::Plus:  return Parameter::Addition;
        case Token::Minus: return Parameter::Subtraction;
        case Token::Asterisk: return Parameter::Multiplication;
        case Token::Slash: return Parameter::Division;

        case Token::LessThan: return Parameter::LessThan;
        case Token::LessEqual: return Parameter::LessEqual;
        case Token::GreaterThan: return Parameter::GreaterThan;
        case Token::GreaterEqual: return Parameter::GreaterEqual;

        case Token::LogicalAnd: return Parameter::LogicalAnd;
        case Token::LogicalOr: return Parameter::LogicalOr;
        case Token::LogicalXor: return Parameter::LogicalXor;
        default: throw std::runtime_error("unhandled token-type in "
                                "tokenTypeToParameterType (Token::Type tok)");
        };
}

template <
        typename AcceptedTokens,
        boost::optional<Parameter> descendant (TokenIterator it,
                   TokenIterator end,
                   TokenIterator &behind
        ),
        int strictArgCount
>
boost::optional<Parameter> parse_term_tpl (
        TokenIterator it,
        TokenIterator end,
        TokenIterator &behind
) {
        using boost::optional;
        /*
                a * b * c * d
                (* a (* b (* c d)))  // <-- the more intuitive way, but wrong
                                            for any left-associative operation
                (* (* (* a b) c) d)  // <-- right
        */

        if (it == end)
                return false;

        const optional<Parameter> first = descendant(it, end, behind);
        if (!first)
                return false;
        it = behind;

        Parameter ret = *first;
        Parameter prev = *first;

        int argCount = 1;
        while (it != end) {
                optional<Parameter> next;

                bool any = false;
                for (unsigned int i=0;
                     i<sizeof(AcceptedTokens::tokens)
                       / sizeof(AcceptedTokens::tokens[0]);
                      ++i
                ) {
                        if (it->type() == AcceptedTokens::tokens[i]) {
                                ret.setType(
                                        tokenTypeToParameterType(it->type()));
                                any = true;
                                break;
                        }
                }
                if (!any) {
                        break;
                }
                ++argCount;

                behind = ++it;
                next = descendant(it, end, behind);
                if (!next)
                        return false;
                it = behind;

                ret.setLhs(prev);
                ret.setRhs(*next);
                prev = ret;
        }

        if (strictArgCount!=-1 && argCount!=strictArgCount) {
                return false;
        }
        return ret;
}


// It will be better with variadic templates.
struct AddTokens { static const Token::Type tokens []; };
const Token::Type AddTokens::tokens [] = { Token::Asterisk, Token::Slash };

struct MulTokens { static const Token::Type tokens []; };
const Token::Type MulTokens::tokens [] = { Token::Plus, Token::Minus };

struct RelTokens { static const Token::Type tokens []; };
const Token::Type RelTokens::tokens [] = { Token::LessThan,
                                           Token::LessEqual,
                                           Token::GreaterThan,
                                           Token::GreaterEqual
                                         };

struct LogTokens { static const Token::Type tokens []; };
const Token::Type LogTokens::tokens [] = { Token::LogicalAnd,
                                           Token::LogicalOr,
                                           Token::LogicalXor
                                         };
} // namespace {

boost::optional<Parameter> parse_term (TokenIterator it, TokenIterator end,
                                       TokenIterator &behind)
{
        return parse_term_tpl<AddTokens, parse_factor, -1>(it, end, behind);
}

boost::optional<Parameter> parse_expr (TokenIterator it, TokenIterator end,
                                       TokenIterator &behind)
{
        return parse_term_tpl<MulTokens, parse_term, -1>(it, end, behind);
}

boost::optional<Parameter> parse_rel(TokenIterator it, TokenIterator end,
                                     TokenIterator &behind)
{
        return parse_term_tpl<RelTokens, parse_expr, 2>(it, end, behind);
}

boost::optional<Parameter> parse_logical(TokenIterator it, TokenIterator end,
                                     TokenIterator &behind)
{
        return parse_term_tpl<LogTokens, parse_rel, -1>(it, end, behind);
}

}

