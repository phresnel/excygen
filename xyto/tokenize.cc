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

#include <iostream>

#include "parse_helpers.hh"
#include "tokenize.hh"

namespace xyto { 

TokenVector tokenize(const char *code) {
        TokenVector tokens;
        typedef CodeIterator CI;
        for (CI it=code; *it!='\0'; ++it) {
                const char c = *it;

                if (c == '-'
                    && it.can_peek(1) && it.peek(1)=='-'
                    && it.can_peek(2) && it.peek(2)=='>'
                ) {
                        const CI begin = it;
                        ++it; ++it;
                        tokens.push_back (Token(Token::TransformTo, begin, it.next()));
                } else if (is_alpha(c) || c=='#') {
                        const CI begin = it;
                        CI prev = it;
                        ++it;
                        for (; *it!='\0' && is_alnum(*it); prev=it, ++it) {
                        }
                        tokens.push_back (Token(Token::Identifier, begin, it));
                        it = prev;
                } else if (is_num(c)/* ||
                           (c=='-' && it.can_peek(1) && is_num(it.peek(1)))*/) {
                        const CI begin = it;
                        CI prev = it;

                        if (c=='-') ++it;
                        for (; *it!='\0' && is_num(*it); prev=it, ++it) {}
                        if (*it == '.') {
                                ++it;
                                if (!is_num(*it)) {
                                        std::cerr << "tokenization error in "
                                             << "line " << it.row()
                                             << ", column " << it.column()
                                             << ": '" << *it << "', "
                                             << "expected number" << std::endl;
                                        return TokenVector();
                                }
                                for (; *it!='\0' && is_num(*it); prev=it, ++it) {
                                }
                                tokens.push_back (Token(Token::Real, begin, it));
                                it = prev;
                        } else {
                                tokens.push_back (Token(Token::Integer, begin, it));
                                it = prev;
                        }
                } else if (c == '"') {
                        std::cout << "parsing string ... \n";
                        CI prev = it;

                        ++it;
                        std::string str = "";
                        const CI begin = it;

                        for(;*it != '\0' && *it!='"'; ++it) {
                                if (*it == '\\') {
                                        if (!it.can_peek(1)) {
                                                std::cerr << "tokenization error in "
                                                     << "line " << it.row()
                                                     << ", column " << it.column()
                                                     << ": "
                                                     << "unexpected end-of-file"
                                                     << " after escape-character"
                                                     << std::endl;
                                                return TokenVector();
                                        }
                                        if (it.peek(1) == '\\') {
                                                str += '\\';
                                        } else if (it.peek(1) == '"') {
                                                str += '"';
                                        } else {
                                                std::cerr << "tokenization error in "
                                                     << "line " << it.row()
                                                     << ", column " << it.column()
                                                     << ": "
                                                     << "unknown escape sequence \""
                                                     << "\\" << it.peek(1) << "\""
                                                     << std::endl;
                                                return TokenVector();
                                        }
                                        ++it;
                                } else {
                                        str += *it;
                                }
                        }

                        if (*it == '\0') {
                                std::cerr << "tokenization error in "
                                     << "line " << it.row()
                                     << ", column " << it.column()
                                     << ": '" << *it << "', "
                                     << "unexpected end-of-file" << std::endl;
                                return TokenVector();
                        }

                        tokens.push_back (Token(Token::String, begin, it.next(), str));
                } else if (c == '<') {
                        if (it.can_peek(1) && it.peek(1) == '=') {
                                tokens.push_back (
                                        Token(Token::LessEqual, it, it.next(2)));
                                ++it;
                        } else {
                                tokens.push_back (
                                    Token(Token::LessThan, it, it.next()));
                        }
                } else if (c == '>') {
                        if (it.can_peek(1) && it.peek(1) == '=') {
                                tokens.push_back (
                                    Token(Token::GreaterEqual, it, it.next(2)));
                                ++it;
                        } else {
                                tokens.push_back (
                                    Token(Token::GreaterThan, it, it.next()));
                        }
                } else if (c == ':') {
                        tokens.push_back (Token(Token::Colon, it, it.next()));
                } else if (c == '=') {
                        tokens.push_back (Token(Token::Equals, it, it.next()));
                } else if (c == ';') {
                        tokens.push_back (Token(Token::Semicolon, it, it.next()));
                } else if (c == '(') {
                        tokens.push_back (Token(Token::LeftParen, it, it.next()));
                } else if (c == ')') {
                        tokens.push_back (Token(Token::RightParen, it, it.next()));
                } else if (c == '[') {
                        tokens.push_back (Token(Token::LeftBracket, it, it.next()));
                } else if (c == ']') {
                        tokens.push_back (Token(Token::RightBracket, it, it.next()));
                } else if (c == ',') {
                        tokens.push_back (Token(Token::Comma, it, it.next()));
                } else if (c == '+') {
                        tokens.push_back (Token(Token::Plus, it, it.next()));
                } else if (c == '-') {
                        tokens.push_back (Token(Token::Minus, it, it.next()));
                } else if (c == '*') {
                        tokens.push_back (Token(Token::Asterisk, it, it.next()));
                } else if (c == '/') {
                        tokens.push_back (Token(Token::Slash, it, it.next()));
                } else if (c == '&' && it.can_peek(1) && it.peek(1) == '&') {
                        tokens.push_back (
                                Token(Token::LogicalAnd, it, it.next(2)));
                        ++it;
                } else if (c == '|' && it.can_peek(1) && it.peek(1) == '|') {
                        tokens.push_back (
                                Token(Token::LogicalOr, it, it.next(2)));
                        ++it;
                } else if (c == 'x' &&
                           it.can_peek(1) && it.peek(1) == 'o' &&
                           it.can_peek(2) && it.peek(2) == 'r'
                ) {
                        tokens.push_back (
                                Token(Token::LogicalOr, it, it.next(3)));
                        ++it; ++it;
                } else if (!is_white(c)) {
                        std::cerr << "tokenization error in "
                             << "line " << it.row() << ", column " << it.column()
                             << ": '" << *it << "'" << std::endl;
                        return TokenVector();
                }
        }
        return tokens;
}

}
