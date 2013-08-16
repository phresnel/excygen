// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef TOKEN_HH_INCLUDED_20130816
#define TOKEN_HH_INCLUDED_20130816

#include <string>
#include <vector>


// -- Tokenization ---------------------------------------------------------------------------------
namespace excyrender { namespace Nature { namespace Et1 {

    using string = std::string;
    using iterator = string::const_iterator;
    using std::vector;

    enum TokenKind {
        Integer,
        Identifier,
        LParen, RParen,
        Plus, Minus,
        Asterisk, Slash,
        Comma
    };

    struct Token {
        TokenKind kind;
        iterator from, to;

        bool operator== (const char *c_str) const {
            return string(from,to) == c_str;
        }

        bool operator!= (const char *c_str) const {
            return !(*this == c_str);
        }

        operator string() const {
            return string(from,to);
        }

        Token (TokenKind kind, iterator from, iterator to) : kind(kind), from(from), to(to) {}
    };

    using token_iter = vector<Token>::const_iterator;

    std::ostream& operator<< (std::ostream &os, Token const &tok);
    std::ostream& operator<< (std::ostream &os, vector<Token> const &toks);

    std::vector<Token> tokenize(std::string const &str);

} } }

#endif // TOKEN_HH_INCLUDED_20130816
