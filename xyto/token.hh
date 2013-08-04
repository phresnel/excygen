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

#ifndef TOKEN_HH_INCLUDED_20100726
#define TOKEN_HH_INCLUDED_20100726

#include <vector>
#include <string>
#include "codeiterator.hh"

namespace xyto { 

class Token {
public:
        enum Type {
                TransformTo, // -->
                Equals,
                Colon, Semicolon, Comma,
                LeftParen, RightParen,     // ()
                LeftBracket, RightBracket, // []
                Identifier, Real, Integer, String,

                Plus, Minus,    // + -
                Asterisk, Slash, // * /

                LessThan, LessEqual,
                GreaterThan, GreaterEqual,

                LogicalAnd, LogicalOr, LogicalXor
        };

        Token (Type type_,
               const CodeIterator &from,
               const CodeIterator &to
        );
        Token (Type type_,
               const CodeIterator &from,
               const CodeIterator &to,
               const std::string &value
        );

        Token (Token const &rhs);
        Token& operator= (Token const &rhs);

        Type type() const;
        std::string value() const;
        double valueAsReal () const;
        int valueAsInteger () const;
        CodeIterator from() const;
        CodeIterator to() const;
private:
        Token();
        Type type_;
        CodeIterator from_, to_;
        std::string value_;
};

typedef std::vector<Token> TokenVector;
typedef TokenVector::const_iterator TokenIterator;

}

#endif // TOKEN_HH_INCLUDED_20100726
