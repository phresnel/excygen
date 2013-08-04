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

#ifndef CODEITERATOR_HH_INCLUDED_20100726
#define CODEITERATOR_HH_INCLUDED_20100726

#include <boost/optional.hpp>

namespace xyto { 

class CodeIterator {
public:
        CodeIterator()
        : curr()
        , row_(0)
        , column_(0) {}

        CodeIterator(char const * val)
        : curr(val)
        , row_(0)
        , column_(0)
        {}

        CodeIterator(CodeIterator const &rhs)
        : curr(rhs.curr)
        , row_(rhs.row_)
        , column_(rhs.column_)
        {}

        bool operator!= (CodeIterator const &rhs) {
                return row_ != rhs.row_
                    || column_ != rhs.column_
                    ;
        }
        CodeIterator operator= (CodeIterator const &rhs) {
                curr = rhs.curr;
                row_ = rhs.row_;
                column_ = rhs.column_;
                return *this;
        }
        CodeIterator& operator ++ () {
                inc();
                return *this;
        }
        CodeIterator operator ++ (int) {
                CodeIterator ret = *this;
                inc();
                return ret;
        }
        char peek (unsigned int p) {
                CodeIterator ret = *this;
                while (p!=0) {
                        ++ret;
                        --p;
                }
                return *ret;
        }
        bool can_peek (unsigned int peek) {
                CodeIterator ret = *this;
                peek--; // <-- This is so that \0 can not be peeked
                while ((peek!=0) && (*ret!='\0')) {
                        ++ret;
                        --peek;
                }
                return peek == 0;
        }
        CodeIterator next(unsigned int n=1) const {
                CodeIterator foo(*this);
                while (n) {
                        ++foo;
                        --n;
                }
                return foo;
        }
        char operator *() const {
                return *curr;
        }

        unsigned int row() const { return row_; }
        unsigned int column() const { return column_; }

private:
        const char *curr;
        unsigned int row_, column_;
        void inc() {
                if (*curr == '\n') {
                        ++row_;
                        column_ = 0;
                } else {
                        ++column_;
                }
                ++curr;
        }
};

} 

#endif // CODEITERATOR_HH_INCLUDED_20100726
