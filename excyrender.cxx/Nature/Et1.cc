// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Et1.hh"
#include <stdexcept>
#include "optional.hh"

namespace excyrender { namespace Nature { namespace Et1 { namespace {

    using string = std::string;
    using iterator = string::const_iterator;

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

    int tokenize(std::string const &str)
    {
        std::cout << "tokenizing [" << str << "]\n";
        for (auto it = str.begin(), end=str.end(); it!=end; ) {
            if (auto oit = integer(it, end)) {
                std::cout << "[int:" << string(it, *oit) << "]\n";
                it = *oit;
            } else if (auto oit = operator_(it, end)) {
                std::cout << "[op:" << string(it, *oit) << "]\n";
                it = *oit;
            } else if (auto oit = identifier(it, end)) {
                std::cout << "[id:" << string(it, *oit) << "]\n";
                it = *oit;
            } else {
                throw std::runtime_error("tokenization failure: ..." + string(it, end));
            }
        }
        return 0;
    }

} } } }


namespace excyrender { namespace Nature { namespace Et1 {

HeightFunction compile (std::string const &code) {
    const auto tokens = tokenize(code);
    throw std::runtime_error("Et1::compile() not fully implemented");
}

} } }
