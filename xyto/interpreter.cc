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

#include <boost/optional.hpp>
#include "pattern.hh"
#include "production.hh"
#include "xyto_ios.hh"
#include <iostream>
#include <stdexcept>

namespace xyto {

namespace {
/*
// Returns the number of matched Segments *in the axiom*.
unsigned int match (Pattern const &pattern,
                 Pattern const &axiom,
                 int axiomIndex
) {
        if (axiomIndex<0)
                return 0;

        if (pattern <= axiom.subset(axiomIndex)) {
                // The following will return the number of matched letters
                // in the axiom.
                // It takes into account that a production "A A" also matches
                // an axiom "A [xxx] A", which is according to ABoP, p.32.
                unsigned int pIndex = 0, aIndex = axiomIndex;
                while (pIndex < pattern.size()) {
                        if (pattern[pIndex].type() == Segment::Letter
                          && axiom[aIndex].type() == Segment::Branch) {
                                ++aIndex;
                        } else {
                                ++aIndex;
                                ++pIndex;
                        }
                }
                return aIndex - axiomIndex;
        }
        return 0;
}*/

// Returns the number of matched Segments *in the axiom*.
unsigned int match (Production const &production,
                 Pattern const &axiom,
                 int axiomIndex
) {
        const ProductionHeader &header = production.header();

        unsigned int mainLen = match (header.pattern(),
                                      axiom.subset(axiomIndex),
                                      false);
        if (!mainLen)
                return 0;

        if (!header.leftContext().empty()) {
                Pattern const & ct = header.leftContext();
                if (!rmatch(ct, axiom.subset(0,axiomIndex), true))
                        return 0;
        }
        if (!header.rightContext().empty()) {
                Pattern const & ct = header.rightContext();
                if (!match(ct, axiom.subset(mainLen), true))
                        return 0;
        }

        // COND

        return mainLen;
}


void fillStack (
        Pattern const &pattern,
        Pattern const &axiom, unsigned int axiomIndex, int axiomOffset,
        std::vector<Parameter> &stack
) {
        // Step 0) Reap values from axiom.
        /*
            axiom:       x(1,2)
            production:  x(#0,#1) -> x(#1,#0)
        */
        for (unsigned int i=0; i<pattern.size(); ++i) {
                again:
                Segment const &sym = pattern[i];
                const unsigned int axiom_i = (i+axiomIndex)+axiomOffset;
                if (axiom_i >= axiom.size()) {
                        std::cerr << "internal error: divergence of "
                                "segment-types within function "
                                "interpreter.cc:fillStack(.) ["
                                << "'"<<sym<<"']"
                                << std::endl;
                        return;
                }
                Segment const &xsym = axiom[axiom_i];

                if (sym.type() == Segment::Letter
                  && xsym.type() == Segment::Branch
                ) {
                        ++axiomIndex;
                        goto again;
                }
                if (sym.type() != xsym.type()) {
                        std::cerr << "internal error: divergence of "
                                "segment-types within function "
                                "interpreter.cc:fillStack(.) ["
                                << "'"<<sym<<"' / '"<<xsym<<"'"
                                << "]"
                                << std::endl;
                        return;
                }

                switch (sym.type()) {
                case Segment::Letter: {
                        ParameterList const &paramList = sym.parameterList();
                        ParameterList const &xparamList = xsym.parameterList();
                        for (unsigned int p=0; p<paramList.size(); ++p) {
                                Parameter const &param = paramList[p];
                                Parameter const &xparam = xparamList[p];

                                int const paramIndex = param.parameterIndex();
                                if (paramIndex >= (int)stack.size()) {
                                        stack.resize (paramIndex+1);
                                }

                                stack[paramIndex] = xparam;
                        }
                } break;
                case Segment::Branch: {
                        fillStack(sym.branch(),
                                  xsym.branch(), 0, 0,
                                  stack);
                } break;
                }
        }
}

Parameter applyStack (Parameter const &pattern, std::vector<Parameter> const &stack);
ParameterList applyStack (ParameterList const &pattern, std::vector<Parameter> const &stack);
Segment applyStack (Segment const &symbol, std::vector<Parameter> const &stack);
Pattern applyStack (Pattern const &pattern, std::vector<Parameter> const &stack);



Parameter fold (Parameter::Type op, Parameter const &lhs, Parameter const &rhs)
{
        Parameter ret;

        // Same rule for floating point promotion as in C++
        if (lhs.type() == Parameter::Integer &&
            rhs.type() == Parameter::Integer) {
                ret.setType (Parameter::Integer);

                const int L = lhs.integer();
                const int R = rhs.integer();

                switch (op) {
                case Parameter::ParameterIndex:
                case Parameter::Integer:
                case Parameter::Real:
                case Parameter::Identifier:
                case Parameter::Negate:
                //case Parameter::Constant:
                        break; /*<-- fall to error. */

                case Parameter::Multiplication:
                        ret.setInteger(L * R);
                        return ret;
                case Parameter::Division:
                        ret.setInteger(L / R);
                        return ret;
                case Parameter::Addition:
                        ret.setInteger(L + R);
                        return ret;
                case Parameter::Subtraction:
                        ret.setInteger(L - R);
                        return ret;
                case Parameter::LessThan:
                        ret.setInteger(L < R);
                        return ret;
                case Parameter::LessEqual:
                        ret.setInteger(L <= R);
                        return ret;
                case Parameter::GreaterThan:
                        ret.setInteger(L > R);
                        return ret;
                case Parameter::GreaterEqual:
                        ret.setInteger(L >= R);
                        return ret;
                case Parameter::LogicalAnd:
                        ret.setInteger(L && R);
                        return ret;
                case Parameter::LogicalOr:
                        ret.setInteger(L || R);
                        return ret;
                case Parameter::LogicalXor:
                        ret.setInteger(!!L != !!R);
                        return ret;
                }
        } else {
                ret.setType (Parameter::Real);

                const double L = lhs.type()==Parameter::Real
                               ? lhs.real()
                               : lhs.integer();
                const double R = rhs.type()==Parameter::Real
                               ? rhs.real()
                               : rhs.integer();

                switch (op) {
                case Parameter::ParameterIndex:
                case Parameter::Integer:
                case Parameter::Real:
                case Parameter::Identifier:
                case Parameter::Negate:
                //case Parameter::Constant:
                        break; /*<-- fall to error. */

                case Parameter::Multiplication:
                        ret.setReal(L * R);
                        return ret;
                case Parameter::Division:
                        ret.setReal(L / R);
                        return ret;
                case Parameter::Addition:
                        ret.setReal(L + R);
                        return ret;
                case Parameter::Subtraction:
                        ret.setReal(L - R);
                        return ret;
                case Parameter::LessThan:
                        ret.setType(Parameter::Integer);
                        ret.setInteger(L < R);
                        return ret;
                case Parameter::LessEqual:
                        ret.setType(Parameter::Integer);
                        ret.setInteger(L <= R);
                        return ret;
                case Parameter::GreaterThan:
                        ret.setType(Parameter::Integer);
                        ret.setInteger(L > R);
                        return ret;
                case Parameter::GreaterEqual:
                        ret.setType(Parameter::Integer);
                        ret.setInteger(L >= R);
                        return ret;
                case Parameter::LogicalAnd:
                case Parameter::LogicalOr:
                case Parameter::LogicalXor:
                        throw std::runtime_error("logical operator invoked "
                                                 "for real-valued operands");
                }
        }

        throw std::runtime_error("unhandled parameter-type in fold(.)");
}


Parameter applyStack (Parameter const &param,
                      std::vector<Parameter> const &stack
) {
        switch (param.type()) {
        case Parameter::ParameterIndex:
                return stack[param.parameterIndex()];

        case Parameter::Integer:
        case Parameter::Real:
                return param;
        case Parameter::Identifier:
                break; /*<-- fall to error. */

        case Parameter::Negate: {
                Parameter ret = applyStack(param.unaryParameter(), stack);
                if (ret.type() == Parameter::Integer)
                        ret.setInteger(-ret.integer());
                else if (ret.type() == Parameter::Real)
                        ret.setInteger(-ret.real());
                else
                        throw std::runtime_error("unhandled parameter-type "
                                                 "for negation in applyStack()"
                                                 );
                return ret;
        } break;

        case Parameter::Multiplication: case Parameter::Division:
        case Parameter::Addition:       case Parameter::Subtraction:

        case Parameter::LessThan:    case Parameter::LessEqual:
        case Parameter::GreaterThan: case Parameter::GreaterEqual:

        case Parameter::LogicalAnd:
        case Parameter::LogicalOr:
        case Parameter::LogicalXor:
                /*
                Parameter ret (param);
                ret.setLhs (applyStack (param.lhs(), stack));
                ret.setRhs (applyStack (param.rhs(), stack));
                return ret;
                //*/
                //for funny results do not apply any calculations here, but
                //instead use commented out code above. The formulas would be
                //inlined, e.g.
                //        f(x) --> f(x+1) --> f((x+1)+1) --> f(((x+1)+1)+1...


                return fold (param.type(),
                             applyStack (param.lhs(), stack),
                             applyStack (param.rhs(), stack));
        }

        throw std::runtime_error("unhandled parameter-type in applyStack(.)");
}



/*ParameterList applyStack (ParameterList const &pattern, std::vector<Parameter> const &stack) {
}*/



Pattern applyStack (Pattern const &pattern, std::vector<Parameter> const &stack) {
        Pattern ret;
        for (unsigned int i=0; i<pattern.size(); ++i) {
                ret.push_back(applyStack(pattern[i], stack));
        }
        return ret;
}




Segment applyStack (Segment const &symbol, std::vector<Parameter> const &stack) {
        Segment ret = symbol;

        switch (symbol.type()) {
        case Segment::Letter: {
                ParameterList const &sourceParams = symbol.parameterList();
                ParameterList &rparams = ret.parameterList();

                for (unsigned int p=0; p<sourceParams.size(); ++p) {
                        Parameter const &param = sourceParams[p];
                        rparams[p] = applyStack (param, stack);
                }
        } break;
        case Segment::Branch: {
                ret.setBranch (applyStack (symbol.branch(), stack));
        } break;
        }

        return ret;
}

} }

#include "portable_rng/kiss.hh"

namespace xyto {

Parameter fold (Parameter const &param) {
        try {
                return applyStack (param, std::vector<Parameter>());
        } catch (...) {
                return param;
        }
}
Pattern fold (Pattern const &param) {
        try {
                return applyStack (param, std::vector<Parameter>());
        } catch (...) {
                return param;
        }
}

boost::optional<Pattern> apply(std::vector<Production> const &prods,
                               Pattern const &axiom,
                               portable_rng::marsaglia::UNI &rng
) {
        using boost::optional;
        bool axiomWasTweaked = false;
        Pattern ret;
        for (unsigned int A=0; A<axiom.size(); ) {
                bool any = false;
                for (unsigned int P=0; P<prods.size(); ++P) {
                        const int len = match(prods[P], axiom, A);
                        const bool doesMatch = len > 0;
                        if (doesMatch) {

                                const ProductionHeader &header
                                                         = prods[P].header();
                                const Pattern &lcPattern = header.leftContext();
                                const Pattern &rcPattern = header.rightContext();
                                const Pattern &mPattern  = header.pattern();
                                const Pattern &body = prods[P].pickBody(rng).pattern();

                                std::vector<Parameter> stack(16);

                                // Fill stack with values from axiom.
                                // E.g., axiom = "a(x) < b(y) c(z) > d(a)"

                                fillStack (lcPattern, axiom, A, -lcPattern.size(), stack);
                                fillStack (mPattern,  axiom, A, 0, stack);
                                fillStack (rcPattern, axiom, A, mPattern.size(), stack);

                                // Check condition.
                                if (header.condition()) {
                                        const int cond =
                                                applyStack(*header.condition(),
                                                           stack).integer();
                                        if (!cond)
                                                continue;
                                }

                                const Pattern tmp = applyStack(body, stack);
                                for (unsigned int i=0; i<tmp.size(); ++i)
                                        ret.push_back(tmp[i]);

                                A += len;
                                any = true;
                                break;
                        }
                }
                if (any) {
                        // NOTE: this is actually wrong. in many cases, doesMatch will be true,
                        //       but the resultant axiom wasn't really tweaked
                        axiomWasTweaked = true;
                } else {
                        if (axiom[A].type() == Segment::Branch) {
                                optional<Pattern> sub = apply(prods,
                                                              axiom[A].branch(),
                                                              rng);
                                //std::cout << ">" << axiom[A].branch() << std::endl;
                                if (sub) {
                                        axiomWasTweaked = true;
                                        Segment branch = axiom[A];
                                        branch.setBranch(*sub);
                                        ret.push_back(branch);
                                        /*std::cout << "!" << *sub << std::endl;
                                        std::cout << "=" << axiom[A].branch() << std::endl;
                                        std::cout << "=" << branch << std::endl;*/
                                } else {
                                        ret.push_back (axiom[A]);
                                }
                        } else {
                                ret.push_back (axiom[A]);
                        }
                        //std::cout << ret[ret.size()-1] << " == " << axiom[A] << std::endl;
                        ++A;
                }
        }

        if (axiomWasTweaked)
                return ret;
        return boost::optional<Pattern>();
}

}


