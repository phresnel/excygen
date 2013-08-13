// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Et1.hh"
#include "optional.hh"
#include <stdexcept>
#include <vector>

namespace excyrender { namespace Nature { namespace Et1 { namespace {

    using string = std::string;
    using iterator = string::const_iterator;
    using std::vector;


    // -- Token ------------------------------------------------------------------------------------
    enum TokenKind {
        Integer,
        Operator,
        Identifier
    };

    struct Token {
        TokenKind kind;
        iterator from, to;

        Token (TokenKind kind, iterator from, iterator to) : kind(kind), from(from), to(to) {}
    };


    std::ostream& operator<< (std::ostream &os, Token const &tok) {
        switch (tok.kind) {
        case Integer: os << "[int:"; break;
        case Operator: os << "["; break;
        case Identifier: os << "[id:"; break;
        }
        return os << string(tok.from, tok.to) << "] ";
    }

    std::ostream& operator<< (std::ostream &os, vector<Token> const &toks) {
        for (auto tok : toks) {
            os << tok;
        }
        return os;
    }

    // -- Tokenizer Helpers ------------------------------------------------------------------------
    namespace detail {

        bool is_digit(char c) {
            return c>='0' && c<='9'; // C++ guarantees that 0..9 are contiguous.
        }

        bool is_uppercase_letter(char c) {
            switch (c) {
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
            case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
            case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
            case 'V': case 'W': case 'X': case 'Y': case 'Z':
                return true;
            }
            return false;
        }

        bool is_lowercase_letter(char c) {
            switch (c) {
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
            case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
            case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
            case 'v': case 'w': case 'x': case 'y': case 'z':
                return true;
            }
            return false;
        }

        bool is_letter(char c) {
            return is_uppercase_letter(c) || is_lowercase_letter(c);
        }

        optional<iterator> integer(iterator it, iterator end) {
            int sign = 1;
            while (it != end) {
                if (*it == '-') {
                    sign = -sign;
                    ++it;
                } else if (*it == '+') {
                    ++it;
                } else {
                    break;
                }
            }
            if (it==end || !is_digit(*it))
                return optional<iterator>();

            while (is_digit(*it)) {
                ++it;
            }
            return it;
        }

        optional<iterator> operator_(iterator it, iterator end)
        {
            if (*it == '+'
                    || *it == '-'
                    || *it == '*'
                    || *it == '/'
                    || *it == '(' || *it == ')'
                    )
            {
                return ++it;
            }
            return optional<iterator>();
        }

        optional<iterator> identifier(iterator it, iterator end) {
            if (!is_letter(*it))
                return optional<iterator>();
            ++it;
            while (it!=end && (is_letter(*it) || is_digit(*it)))
                ++it;
            return it;
        }
    }


    // -- Tokenizer --------------------------------------------------------------------------------
    std::vector<Token> tokenize(std::string const &str)
    {
        using namespace detail;
        std::vector<Token> tokens;

        std::cout << "tokenizing [" << str << "]\n";
        for (auto it = str.begin(), end=str.end(); it!=end; ) {
            if (auto oit = integer(it, end)) {
                tokens.emplace_back(Integer, it, *oit);
                it = *oit;
            } else if (auto oit = operator_(it, end)) {
                tokens.emplace_back(Operator, it, *oit);
                it = *oit;
            } else if (auto oit = identifier(it, end)) {
                tokens.emplace_back(Identifier, it, *oit);
                it = *oit;
            } else {
                throw std::runtime_error("tokenization failure: ..." + string(it, end));
            }
        }
        return tokens;
    }

} } } }


namespace excyrender { namespace Nature { namespace Et1 {

HeightFunction compile (std::string const &code) {
    const auto tokens = tokenize(code);
    std::cout << tokens << '\n';
    throw std::runtime_error("Et1::compile() not fully implemented");
}

} } }
