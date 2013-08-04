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
#include "production.hh"
#include "lsystem.hh"
#include "portable_rng/kiss.hh"
#include "xyto_ios.hh"

namespace xyto { 

Pattern LSystem::axiom () const {
        return axiom_;
}


void LSystem::setAxiom (const Pattern &axiom) {
        axiom_ = axiom;
}


Production LSystem::production(unsigned int index) const {
        return productions_[index];
}


void LSystem::setProductions (std::vector<Production> const &rhs) {
        productions_ = rhs;
}


std::vector<Production> LSystem::productions() const {
        return productions_;
}


Constant LSystem::constant (std::string const &name) const {
        if (constantExists(name))
                return constants_.find(name)->second;
        std::cerr << "runtime error: constant '" << name << "' not found."
                  << std::endl;
        return Constant();
}


void LSystem::addConstant (Constant const &constant) {
        constants_[constant.name()]  = constant;
}


bool LSystem::constantExists (std::string const &name) const {
        return 0 != constants_.count(name);
}


void LSystem::setConstants (std::map<std::string, Constant> const & constants){
        this->constants_ = constants;
}


std::vector<Constant> LSystem::constants () const {
        typedef std::map<std::string, Constant>::const_iterator iter;
        std::vector<Constant> ret;
        for (iter it=constants_.begin(); it != constants_.end(); ++it) {
                ret.push_back(it->second);
        }
        return ret;
}


Pattern LSystem::run (
        unsigned int count
) const {
        portable_rng::marsaglia::UNI rng (1,2,3,4);
        rng.skip(1024);
        return run (rng, count);
}


Pattern LSystem::run (
        portable_rng::marsaglia::UNI &rng,
        unsigned int count
) const {
        boost::optional<Pattern> apply(
                std::vector<Production> const &,
                Pattern const &,
                portable_rng::marsaglia::UNI &rng
        );

        Pattern pat = axiom_;
        for (unsigned int step=0; step<count; ++step) {

                boost::optional<Pattern> next = apply (productions_,
                                                       pat,
                                                       rng);
                //std::cout << "step " << step+1 << ": ";
                if (next) {
                        pat = *next;
                        //std::cout << pat << '\n';
                } else {
                        //std::cout << "<no match>\n";
                        break;
                }
        }
        return pat;
}

}
