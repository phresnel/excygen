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

#ifndef PATTERN_HH_INCLUDED_20100726
#define PATTERN_HH_INCLUDED_20100726

#include <vector>
#include "segment.hh"

namespace xyto { 

class Segment;

class Pattern {
public:
        Pattern();
        Pattern(Pattern const &rhs);
        Pattern& operator= (Pattern const &rhs);


        bool empty() const;
        Pattern subset(unsigned int u) const ;
        Pattern subset(unsigned int from, unsigned int to) const ;



        // -- array/push/pop --------------------------------------------------
        Segment const& operator[] (unsigned int i) const;
        Segment&       operator[] (unsigned int i);

        unsigned int size() const;
        void push_back(Segment const &sym);
        // --------------------------------------------------------------------



        // -- iteration -------------------------------------------------------
        typedef std::vector<Segment>::iterator
                                      iterator;
        typedef std::vector<Segment>::const_iterator
                                      const_iterator;
        typedef std::vector<Segment>::reverse_iterator
                                      reverse_iterator;
        typedef std::vector<Segment>::const_reverse_iterator
                                      const_reverse_iterator;

        iterator               begin()         { return symbols.begin(); }
        const_iterator         begin() const   { return symbols.begin(); }
        iterator               end()           { return symbols.end(); }
        const_iterator         end() const     { return symbols.end(); }
        reverse_iterator       rbegin()        { return symbols.rbegin(); }
        const_reverse_iterator rbegin() const  { return symbols.rbegin(); }
        reverse_iterator       rend()          { return symbols.rend(); }
        const_reverse_iterator rend() const    { return symbols.rend(); }
        // --------------------------------------------------------------------


private:
        std::vector<Segment> symbols;
};

bool operator == (Pattern const &lhs, Pattern const &rhs);
bool operator <= (Pattern const &lhs, Pattern const &rhs);

unsigned int match (Pattern const &pattern,
                    Pattern const &axiom,
                    bool maySkipBranchesAtExtremes);
unsigned int rmatch (Pattern const &pattern,
                     Pattern const &axiom,
                     bool maySkipBranchesAtExtremes);

}

#endif // PATTERN_HH_INCLUDED_20100726
