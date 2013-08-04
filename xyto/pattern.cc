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

#include "pattern.hh"
#include "segment.hh"

namespace xyto { 

Segment const& Pattern::operator[] (unsigned int i) const {
        return symbols[i];
}



Segment & Pattern::operator[] (unsigned int i) {
        return symbols[i];
}



unsigned int Pattern::size() const {
        return symbols.size();
}



void Pattern::push_back(Segment const &sym) {
        symbols.push_back(sym);
}



bool Pattern::empty() const {
        return size() == 0;
}



Pattern::Pattern() {
}



Pattern::Pattern(Pattern const &rhs)
: symbols(rhs.symbols)
{
}



Pattern& Pattern::operator= (Pattern const &rhs) {
        symbols = rhs.symbols;
        return *this;
}



Pattern Pattern::subset(unsigned int u) const {
        Pattern ret;
        for (unsigned int i=u; i<size(); ++i)
                ret.push_back ((*this)[i]);
        return ret;
}



Pattern Pattern::subset(unsigned int from, unsigned int to) const {
        Pattern ret;
        for (unsigned int i=from; i<to; ++i)
                ret.push_back ((*this)[i]);
        return ret;
}



bool operator == (Pattern const &lhs, Pattern const &rhs) {
        if (lhs.size() != rhs.size())
                return false;
        for (unsigned int i=0; i<lhs.size(); ++i) {
                if (lhs[i] != rhs[i])
                        return false;
        }
        return true;
}

}


#include <iostream>
#include "production.hh"
#include "xyto_ios.hh"

namespace xyto { 

bool operator <= (Pattern const &lhs, Pattern const &rhs) {
        /*if (lhs.size()>rhs.size())
                return false;*/
//std::cout << "\n---------------------{{\n" << lhs << "\n";
//std::cout << rhs << std::endl;
/*std::cout << ">>>>>>" << std::endl;
std::cout << "  l: " << lhs << std::endl;
std::cout << "  r: " << rhs << std::endl;*/

        unsigned int index_l, index_r;
        for (index_l=index_r=0;
             true;
             ++index_l, ++index_r
        ) {

                cond:
                if (index_l>=lhs.size() || index_r>=rhs.size())
                        break;

                // e.g. in "axiom: A [A] A;
                //          foo: A A --> B;"
                // the production foo may skip over the branch. See ABoP, p.32
                if (rhs[index_r].type() == Segment::Branch
                 && lhs[index_l].type() != Segment::Branch
                ) {
                        /*std::cout << "uhm" << std::endl;
                        std::cout << "  l: " << lhs[index_l] << std::endl;
                        std::cout << "  r: " << rhs[index_r] << std::endl;*/
                        ++index_r;
                        goto cond;
                }

                if (lhs[index_l] != rhs[index_r])
                        return false;
        }
//std::cout << "}}-----------------\n";
        return index_l == lhs.size();
}




template <typename iterator>
unsigned int match(
        iterator pattern_begin, iterator pattern_end,
        iterator axiom_begin, iterator axiom_end,
        bool maySkipBranchesAtExtremes
) {
        iterator aix = axiom_begin;
        iterator it;
        for (it=pattern_begin; it!=pattern_end; ++it) {
                again:
                if (aix == axiom_end)
                        break;

                const bool isExtreme = aix==axiom_begin||aix==axiom_end;

                if (!isExtreme || maySkipBranchesAtExtremes) {
                        const bool skipBranch = it->type()  == Segment::Letter
                                             && aix->type() == Segment::Branch;
                        if (skipBranch) {
                                ++aix;
                                goto again;
                        }
                }

                if (*it != *aix)
                        return false;
                ++aix;
        }
        const bool fullMatch = it == pattern_end;
        const unsigned int axiomMatchLength = aix-axiom_begin;

        return fullMatch ? axiomMatchLength : 0;
}

unsigned int match (Pattern const &pattern,
                    Pattern const &axiom,
                    bool maySkipBranchesAtExtremes
) {
        return match (pattern.begin(), pattern.end(),
                      axiom.begin(), axiom.end(),
                      maySkipBranchesAtExtremes);
}

unsigned int rmatch (Pattern const &pattern,
                    Pattern const &axiom,
                    bool maySkipBranchesAtExtremes
) {
        return match (pattern.rbegin(), pattern.rend(),
                      axiom.rbegin(), axiom.rend(),
                      maySkipBranchesAtExtremes);
}

}
