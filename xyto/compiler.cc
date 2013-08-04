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

#include <map>
#include <set>
#include <iostream>

//#include "ignore_strict_aliasing" // because of boost::optional
#include <boost/optional.hpp>

#include "production.hh"
#include "tokenize.hh"
#include "token.hh"
#include "parameter.hh"
#include "xyto_ios.hh"
#include "parse_expr.hh"

#include "lsystem.hh"


namespace xyto { 

Pattern fold (Pattern const &);

namespace {

Pattern parse_pattern (TokenIterator it, TokenIterator end,
                       TokenIterator &behind, bool isHeader);
boost::optional<double> parse_probability (TokenIterator it,
                                           TokenIterator end,
                                           TokenIterator &behind);

boost::optional<Pattern> apply_symbol_table (
        Pattern pat,
        bool isHeader,
        std::map<std::string, int> const & symtab,
        std::map<std::string, Constant> const & consttab
);


inline bool hasPrecedenceOver (Production const &lhs, Production const &rhs) {
        const int numContextsLhs =
                !lhs.header().leftContext().empty()
                + !lhs.header().rightContext().empty();
        const int numContextsRhs =
                !rhs.header().leftContext().empty()
                + !rhs.header().rightContext().empty();

        // Always prefer contexts over no context, as well as
        // two contexts over one context.
        if (numContextsLhs > numContextsRhs)
                return true;
        else if (numContextsLhs < numContextsRhs)
                return false;

        // Always prefer larger main-pattern.
        const int patternSizeLhs = lhs.header().pattern().size();
        const int patternSizeRhs = rhs.header().pattern().size();
        if (patternSizeLhs > patternSizeRhs)
                return true;
        else if (patternSizeLhs < patternSizeRhs)
                return false;

        // Prefer larger context.
        const int contextSizeLhs = lhs.header().leftContext().size()
                                 + lhs.header().rightContext().size();
        const int contextSizeRhs = rhs.header().leftContext().size()
                                 + rhs.header().rightContext().size();
        if (contextSizeLhs > contextSizeRhs)
                return true;
        else if (contextSizeLhs < contextSizeRhs)
                return false;

        // Prefer conditioned.
        if (lhs.header().condition() && !rhs.header().condition())
                return true;

        return false;
}



inline bool ambiguous (Production const &lhs, Production const &rhs) {
        return lhs.header().pattern() == rhs.header().pattern()
            && lhs.header().leftContext() == rhs.header().leftContext()
            && lhs.header().rightContext() == rhs.header().rightContext()
            && !hasPrecedenceOver(lhs, rhs)
            && !hasPrecedenceOver(rhs, lhs)
            ;
}



boost::optional<Parameter> parse_parameter (
        TokenIterator it, TokenIterator end, TokenIterator &behind,
        bool isHeader
) {
        if (it == end)
                return boost::optional<Parameter>();
        if (isHeader) {
                if (it->type() == Token::Identifier) {
                        Parameter p;
                        p.setType (Parameter::Identifier);
                        p.setIdentifier (it->value());
                        behind = ++it;
                        return p;
                }
                std::cerr
                  << "error: only a single identifier (e.g. 'id') "
                  << "is allowed as a formal parameter in the "
                  << "header of a production, line "
                  << it->from().row() << ", column "
                  << it->from().column() << std::endl;
                return boost::optional<Parameter>();
        }
        return parse_expr(it, end, behind);
}



boost::optional<ParameterList> parse_parameter_list (
        TokenIterator it, TokenIterator end, TokenIterator &behind,
        bool isHeader
) {
        ParameterList ret;
        if (it == end || it->type() != Token::LeftParen) {
                behind = it;
                return ret;
        }
        ++it;
        if (it == end) {
                std::cerr << "error: unclosed parameter list in line "
                        << (it-1)->to().next().row() << ", column "
                        << (it-1)->to().next().column() << std::endl;
                return boost::optional<ParameterList>();
        }

        if (it->type() == Token::RightParen) {
                ++it;
        } else while (1) {
                TokenIterator behindParam;
                boost::optional<Parameter> p = parse_parameter(
                                          it, end, behindParam,
                                          isHeader);
                if (p) {
                        ret.push_back(*p);
                        it = behindParam;
                } else {
                        std::cerr <<
                           "error: expected parameter in line "
                           << it->from().next().row() << ", column "
                           << it->from().next().column() << std::endl;
                        return boost::optional<ParameterList>();
                }

                if (it == end) {
                        TokenIterator i = it - 1;
                        std::cerr <<
                           "error: unclosed parameter list in line "
                           << i->from().next().row() << ", column "
                           << i->from().next().column() << std::endl;
                        return boost::optional<ParameterList>();
                } else if (it->type() == Token::RightParen) {
                        ++it;
                        break;
                } else if (it->type() == Token::Comma) {
                        ++it;
                }
        }

        behind = it;
        return ret;
}



boost::optional<Segment> parse_segment (
        TokenIterator it, TokenIterator end, TokenIterator &behind,
        bool isHeader
) {
        if (it == end)
                return boost::optional<Segment>();

        if (it->type() == Token::Identifier) {
                Segment sym;
                sym.setType (Segment::Letter);
                sym.setName (it->value());
                ++it;

                TokenIterator behindParams;
                boost::optional<ParameterList> params =
                        parse_parameter_list(it, end, behindParams, isHeader);
                if (!params)
                        return boost::optional<Segment>();
                sym.setParameterList (*params);
                it = behindParams;
                behind = it;
                return sym;
        } else if (it->type() == Token::LeftBracket) {
                const TokenIterator start = it;

                ++it;
                if (it == end
                 || it->type() == Token::Real
                 || it->type() == Token::Integer
                )
                        return boost::optional<Segment>();
                Segment sym;
                sym.setType(Segment::Branch);
                sym.setName("[");
                TokenIterator behindBranch;
                sym.setBranch(parse_pattern(it, end, behindBranch, isHeader));
                it = behindBranch;

                if (it == end || it->type() != Token::RightBracket) {
                        /*std::cerr << "missing ']' after branch started at "
                           << "line " << start->to().row() << ", column "
                           << start->to().column() << std::endl;*/
                        return boost::optional<Segment>();
                }

                behind = ++it;
                return sym;
        } else {
                return boost::optional<Segment>();
        }
}



Pattern parse_pattern (TokenIterator it, TokenIterator end,
                       TokenIterator &behind, bool isHeader) {
        Pattern ret;
        while (boost::optional<Segment> sym = parse_segment(it, end,
                                                            behind, isHeader)
        ) {
                it = behind;
                ret.push_back(*sym);
        }
        behind = it;
        return ret;
}



bool contains_unknowns (Pattern const &pat) {
        for (unsigned int i=0; i<pat.size(); ++i) {
                Segment const & sym = pat[i];

                switch (sym.type()) {
                case Segment::Letter: {
                        ParameterList const &pm = sym.parameterList();

                        for (unsigned int p=0; p<pm.size(); ++p) {
                                if (pm[p].type() != Parameter::Integer
                                 && pm[p].type() != Parameter::Real
                                 //&& pm[p].type() != Parameter::Constant
                                ) {
                                        return true;
                                }
                        }
                        break;
                }
                case Segment::Branch:
                        return contains_unknowns(sym.branch());
                }
        }
        return false;
}



boost::optional<Pattern> parse_axiom (
        TokenIterator it, TokenIterator end,
        TokenIterator &behind,
        const std::map<std::string, Constant> &consttab
) {
        if (it == end ||
            it->type() != Token::Identifier ||
            it->value() != "axiom"
        ) {
                return boost::optional<Pattern>();
        }
        ++it;
        if (it == end || it->type() != Token::Colon) {
                return boost::optional<Pattern>();
        }
        ++it;

        Pattern ret = parse_pattern (it, end, behind, false);
        if (it == behind ||
            behind == end ||
            behind->type() != Token::Semicolon
        ) {
                std::cerr << "error: expected ';' after axiom; see "
                          << "line " << it->from().row() << ", column "
                          << it->from().column()
                          << std::endl;
                return boost::optional<Pattern>();
        }
        const boost::optional<Pattern> nret = apply_symbol_table(
                                ret, false,
                                std::map<std::string, int>(),//empty for axiom
                                consttab);
        if (!nret) {
                return boost::optional<Pattern>();
        }
        ret = fold(*nret);

        std::cout << "<<<<" << ret << std::endl;
        if (contains_unknowns(ret)) {
                std::cerr << "error: axioms may not contain any unknowns; see "
                          << "line " << it->from().row() << ", column "
                          << it->from().column()
                          << std::endl;
                return boost::optional<Pattern>();
        }
        it = behind;
        behind = ++it;
        return ret;
}



bool parse_header_patterns (
        TokenIterator it, TokenIterator end,
        TokenIterator &behind,
        Pattern &leftContext, Pattern &mainPattern, Pattern &rightContext
) {
        const TokenIterator beforePatterns = it;
        TokenIterator behindPattern;
        const Pattern pat = parse_pattern(it, end, behindPattern, true);
        TokenIterator prev = it;
        it = behindPattern;

        if (it != end && it->type() == Token::LessThan) {
                leftContext = pat;
                ++it;
                mainPattern = parse_pattern(it, end, behindPattern, true);
                prev = it; it = behindPattern;

                if (it != end && it->type() == Token::GreaterThan) {
                        ++it;
                        rightContext = parse_pattern(it, end,
                                                     behindPattern, true);
                        prev = it; it = behindPattern;
                }

        } else if (it != end && it->type() == Token::GreaterThan) {
                mainPattern = pat;
                ++it;
                rightContext = parse_pattern(it, end, behindPattern, true);
                prev = it; it = behindPattern;
        } else if (!pat.empty()) {
                mainPattern = pat;
        }

        if (mainPattern.empty()) {
                TokenIterator i = beforePatterns - 1;
                std::cerr <<
                   "error: no (valid) pattern found in line "
                   << i->from().next().row() << ", column "
                   << i->from().next().column() << std::endl;
                return boost::optional<Production>();
        }
        behind = it;

        return true;
}



boost::optional<ProductionHeader> parse_production_header (
        TokenIterator it, TokenIterator end, TokenIterator &behind,
        const std::map<std::string, Constant> &/*consttab*/
) {
        TokenIterator prev = it;

        if (Token::Identifier != it->type()) {
                std::cerr << "error: expected identifier at line "
                        << prev->from().row() << ", column "
                        << prev->from().column() << std::endl;
                return boost::optional<ProductionHeader>();
        }
        const std::string name = it->value();

        // early exit. because when this is the axiom, we get a plethora of
        // error messages for a production without header.
        if (name == "axiom") return boost::optional<ProductionHeader>();

        prev = it;
        ++it;
        // Checkpoint: We have an identifier.

        if (it==end || it->type() != Token::Colon) {
                return boost::optional<ProductionHeader>();
        }
        prev = it;
        ++it;
        // Checkpoint: The identifier is followed by a colon.

        TokenIterator behindMatchPatterns;
        Pattern leftContext, mainPattern, rightContext;

        if (!parse_header_patterns(it, end, behindMatchPatterns,
                                   leftContext, mainPattern, rightContext)
        ) {
                return boost::optional<ProductionHeader>();
        }
        it = behindMatchPatterns;

        if (it == end || (it->type() != Token::TransformTo
                          && it->type() != Token::Colon)
        ) {
                TokenIterator i = it - 1;
                std::cerr <<
                   "error: expected body (starting with '-->') for "
                   " production in line "
                   << i->from().next().row() << ", column "
                   << i->from().next().column() << ", but found "
                   << "'" << i->value() << "'" << std::endl;
                return boost::optional<ProductionHeader>();
        }
        // Checkpoint: We just passed "-->" or ":"
        boost::optional<Parameter> condition;
        if (it->type() == Token::Colon) {
                ++it;

                if (it == end
                    || it->type() != Token::Identifier
                    || it->value() != "if"
                ) {
                        TokenIterator i = it - 1;
                        std::cerr <<
                           "error: expected condition (starting with "
                           "': if(...)' for production in line "
                           << i->from().next().row() << ", column "
                           << i->from().next().column() << ", but found "
                           << "'" << i->value() << "'" << std::endl;
                        return boost::optional<ProductionHeader>();
                }
                ++it;
                if (it == end || it->type() != Token::LeftParen) {
                        TokenIterator i = it - 1;
                        std::cerr <<
                           "error: expected '(' after 'if' in line "
                           << i->from().next().row() << ", column "
                           << i->from().next().column() << ", but found "
                           << "'" << i->value() << "'" << std::endl;
                        return boost::optional<ProductionHeader>();
                }
                ++it;

                //boost::optional<Parameter> cond = ;
                condition = parse_logical(it , end, behind);
                if (!condition) {
                        TokenIterator i = it==end ? it-1 : it;
                        std::cerr <<
                           "error: expected condition within '(' and ')' in "
                           "line "
                           << i->from().next().row() << ", column "
                           << i->from().next().column() << ", but found "
                           << "'" << i->value() << "'" << std::endl;
                        return boost::optional<ProductionHeader>();
                }

                it = behind;
                if (it == end || it->type() != Token::RightParen) {
                        TokenIterator i = it==end ? it-1 : it;
                        std::cerr <<
                           "error: expected ')' after condition in line "
                           << i->from().next().row() << ", column "
                           << i->from().next().column() << ", but found "
                           << "'" << i->value() << "'" << std::endl;
                        return boost::optional<ProductionHeader>();
                }
                ++it;

                if (it == end || it->type() != Token::TransformTo) {
                        TokenIterator i = it==end ? it-1 : it;
                        std::cerr <<
                           "error: expected body (starting with '-->') for "
                           " production in line "
                           << i->from().next().row() << ", column "
                           << i->from().next().column() << ", but found "
                           << "'" << i->value() << "'" << std::endl;
                        return boost::optional<ProductionHeader>();
                }
                ++it;

        } else {
                ++it;
        }
        behind = it;

        ProductionHeader ret;
        ret.setName (name);
        ret.setLeftContext(leftContext);
        ret.setRightContext(rightContext);
        ret.setPattern(mainPattern);
        if (condition) ret.setCondition(*condition);

        return ret;
}



boost::optional<ProductionBody> parse_production_body (
        // foo --> bar  x:
        TokenIterator it, TokenIterator end, TokenIterator &behind
) {
        ProductionBody ret;

        //---------------------------------------------------------
        // Weight
        TokenIterator behindProbability;
        const boost::optional<double> prob =
                        parse_probability(it, end, behindProbability);
        if (prob) {
                if (*prob < 0) {
                        std::cerr
                          << "error: probabilities must be >= 0, see line "
                          << it->to().row() << ", column "
                          << it->to().column() << std::endl;
                        return boost::optional<ProductionBody>();
                }
                it = behindProbability;
                ret.setProbability(*prob);
        }  else {
                ret.setProbability(1);
        }
        //---------------------------------------------------------

        TokenIterator behindPattern;
        const Pattern pat = parse_pattern(it, end, behindPattern, false);
        it = behindPattern;
        if (it == end ||
           (it->type() != Token::Semicolon && it->type() != Token::LeftBracket)
        ) {
                TokenIterator prev = it - 1;
                std::cerr << "error: expected ';' after production in line "
                        << prev->to().row() << ", column "
                        << prev->to().column() << std::endl;
                return boost::optional<ProductionBody>();
        }
        behind = it;

        ret.setPattern (pat);
        return ret;
}




boost::optional<std::vector<ProductionBody> > parse_production_bodies (
        // foo --> bar  x:
        TokenIterator it, TokenIterator end, TokenIterator &behind
) {
        typedef boost::optional<std::vector<ProductionBody> > RET;

        std::vector<ProductionBody> ret;
        TokenIterator behindProd;
        while (boost::optional<ProductionBody> body
                = parse_production_body(it, end, behindProd)
        ) {
                if (it == behindProd || behindProd == end) {
                        TokenIterator prev = it - 1;
                        std::cerr
                            << "error: expected ';' after production in line "
                            << prev->to().row() << ", column "
                            << prev->to().column() << std::endl;
                        return RET();
                }

                it = behindProd;
                ret.push_back (*body);
                if (it->type() == Token::Semicolon) {
                        break;
                }
        }

        if (it == end || it->type() != Token::Semicolon) {
                TokenIterator prev = it - 1;
                std::cerr << "error: expected ';' after production in line "
                        << prev->to().row() << ", column "
                        << prev->to().column() << std::endl;
                return RET();
        }
        ++it;
        behind = it;
        return ret;
}



boost::optional<double> parse_probability (
        TokenIterator it, TokenIterator end, TokenIterator &behind
) {
        if (it == end || it->type() != Token::LeftBracket) {
                return boost::optional<double>();
        }
        ++it;
        if (it == end ||
            (it->type() != Token::Integer && it->type() != Token::Real)
        ) {
                return boost::optional<double>();
        }
        const double p = it->valueAsReal();
        ++it;
        if (it == end || it->type() != Token::RightBracket) {
                std::cout << "fault prob" << std::endl;
                return boost::optional<double>();
        }
        ++it;
        behind = it;
        return p;
}



void compile_symbol_table (
        const Parameter& param,
        std::map<std::string, int> &symtab
) {
        switch (param.type()) {
        case Parameter::Identifier:
                if (!symtab.count(param.identifier())) {
                        const int id_ = symtab.size();
                        symtab[param.identifier()] = id_;
                }
                break;

        case Parameter::Integer:
        case Parameter::Real:
        case Parameter::ParameterIndex:
        //case Parameter::Constant:

        case Parameter::Negate:

        case Parameter::Multiplication: case Parameter::Division:
        case Parameter::Addition:       case Parameter::Subtraction:

        case Parameter::LessThan:    case Parameter::LessEqual:
        case Parameter::GreaterThan: case Parameter::GreaterEqual:

        case Parameter::LogicalAnd:
        case Parameter::LogicalOr:
        case Parameter::LogicalXor:
                /*compile_symbol_table (param.lhs(), symtab);
                compile_symbol_table (param.rhs(), symtab);
                break;*/
                return;
        }
}



void compile_symbol_table (
        const ParameterList& params,
        std::map<std::string, int> &symtab
) {
        for (unsigned int a=0; a<params.size(); ++a) {
                compile_symbol_table (params[a], symtab);
        }
}



void compile_symbol_table (
        const Pattern& pattern,
        std::map<std::string, int> &symtab
) {
        for (unsigned int p=0; p<pattern.size(); ++p) {
                switch (pattern[p].type()) {
                case Segment::Letter:
                        compile_symbol_table(pattern[p].parameterList(),
                                             symtab);
                        break;
                case Segment::Branch:
                        compile_symbol_table(pattern[p].branch(), symtab);
                        break;
                }
        }
}


/*boost::optional<Parameter> apply_symbol_table (
        Parameter param, bool isHeader,
        std::map<std::string, int> const & symtab
);
boost::optional<ParameterList> apply_symbol_table (
        ParameterList params, bool isHeader,
        std::map<std::string, int> const & symtab
);
boost::optional<Pattern> apply_symbol_table (
        Pattern pat, bool isHeader,
        std::map<std::string, int> const & symtab
);*/



boost::optional<Parameter> apply_symbol_table (
        Parameter param,
        bool isHeader,
        std::map<std::string, int> const & symtab,
        std::map<std::string, Constant> const & consttab
) {
        using boost::optional;

        switch (param.type()) {
        case Parameter::Identifier:
                // 0) lookup symbols
                if (!symtab.count(param.identifier())) {

                        // 1) lookup constants
                        if (!consttab.count(param.identifier())) {
                                std::cout << "error: parameter or constant '"
                                        << param.identifier()
                                        << "' unknown." << std::endl;
                                return optional<Parameter>();
                        }

                        param.toConstant(
                                consttab.find(param.identifier())->second);
                } else {
                        if (isHeader && consttab.count(param.identifier())) {
                                std::cout << "warning: parameter '"
                                        << param.identifier()
                                        << "' overrides the constant "
                                        << "'" << param.identifier()
                                        << "'." << std::endl;
                        }
                        param.toParameterIndex(
                                symtab.find(param.identifier())->second);
                }
                break;

        case Parameter::Integer:
        case Parameter::Real:
        case Parameter::ParameterIndex:
        //case Parameter::Constant:
                break;

        case Parameter::Negate: {
                const optional<Parameter>
                    p = apply_symbol_table (param.unaryParameter(), isHeader,
                                            symtab, consttab);
                if (!p)
                        return optional<Parameter>();
                param.setUnaryParameter(*p);
                break;
        }

        case Parameter::Multiplication: case Parameter::Division:
        case Parameter::Addition:       case Parameter::Subtraction:

        case Parameter::LessThan:    case Parameter::LessEqual:
        case Parameter::GreaterThan: case Parameter::GreaterEqual:

        case Parameter::LogicalAnd:
        case Parameter::LogicalOr:
        case Parameter::LogicalXor: {
                const optional<Parameter>
                        lhs = apply_symbol_table (param.lhs(),isHeader,
                                                  symtab,consttab),
                        rhs = apply_symbol_table (param.rhs(),isHeader,
                                                  symtab,consttab);
                if (!lhs || !rhs)
                        return optional<Parameter>();
                param.setLhs(*lhs);
                param.setRhs(*rhs);
                break;
        }
        }

        return param;
}


boost::optional<ParameterList> apply_symbol_table (
        ParameterList params,
        bool isHeader,
        std::map<std::string, int> const & symtab,
        std::map<std::string, Constant> const & consttab
) {
        using boost::optional;
        for (unsigned int a=0; a<params.size(); ++a) {
                const optional<Parameter> param = apply_symbol_table(params[a],
                                                                     isHeader,
                                                                     symtab,
                                                                     consttab);
                if (!param) return optional<ParameterList>();
                params[a] = *param;

        }
        return params;
}



boost::optional<Pattern> apply_symbol_table (
        Pattern pat,
        bool isHeader,
        std::map<std::string, int> const & symtab,
        std::map<std::string, Constant> const & consttab
) {
        using boost::optional;

        for (unsigned int p=0; p<pat.size(); ++p) {
                switch (pat[p].type()) {
                case Segment::Letter: {
                        const optional<ParameterList> params =
                                apply_symbol_table(pat[p].parameterList(),
                                                   isHeader,
                                                   symtab,
                                                   consttab);
                        if (!params) return optional<Pattern>();
                        pat[p].setParameterList(*params);
                } break;
                case Segment::Branch: {
                        boost::optional<Pattern> sub = apply_symbol_table(
                                                             pat[p].branch(),
                                                             isHeader,
                                                             symtab,
                                                             consttab);
                        if (!sub)
                                return boost::optional<Pattern>();
                        pat[p].setBranch (*sub);
                        break;
                } break;
                }
        }
        return pat;
}



boost::optional<Production> parse_production (
      TokenIterator it,
      TokenIterator end,
      TokenIterator &behind,
      std::map<std::string, Constant> const & consttab
) {
        using boost::optional;
        if (it==end) {
                std::cerr << "found nothing" << std::endl;
                return boost::optional<Production>();
        }

        const TokenIterator startIt = it;

        boost::optional<ProductionHeader> header;
        boost::optional<std::vector<ProductionBody> > bodies;

        //---------------------------------------------------------
        // Header
        TokenIterator behindHeader;
        header = parse_production_header(it, end, behindHeader, consttab);
        if (!header) {
                return boost::optional<Production>();
        }
        it = behindHeader;

        if (it == end) {
                TokenIterator i = it - 1;
                std::cerr <<
                   "error: expected body behind '-->' in line "
                   << i->to().next().row() << ", column "
                   << i->to().next().column() << std::endl;
                return boost::optional<Production>();
        }
        //---------------------------------------------------------



        //---------------------------------------------------------
        // Body
        TokenIterator behindBody;
        bodies = parse_production_bodies(it, end, behindBody);
        if (!bodies) {
                return boost::optional<Production>();
        }

        // normalize probabilities
        double sum = 0;
        for (unsigned int i=0; i<bodies->size(); ++i)
                sum += (*bodies)[i].probability();
        if (sum <= 0.000001) {
                std::cerr
                  << "error: the sum of probabilities for production '"
                  << header->name() << "' in line "
                  << startIt->from().next().row() << ", column "
                  << startIt->from().next().column() << " must not be zero."
                  << std::endl;
                return boost::optional<Production>();
        }
        for (unsigned int i=0; i<bodies->size(); ++i)
                (*bodies)[i].setProbability((*bodies)[i].probability() / sum);
        it = behindBody;
        //---------------------------------------------------------


        Production ret;


        //---------------------------------------------------------
        // Compile symbol table.
        {
                // At this point it is ensured that all parameters are
                // identifiers. parse_headers() did the check for us.

                std::map<std::string, int> symtab;
                compile_symbol_table(header->leftContext(), symtab);
                compile_symbol_table(header->pattern(), symtab);
                compile_symbol_table(header->rightContext(), symtab);

                const optional<Pattern>
                     newL = apply_symbol_table(header->leftContext(),
                                               true,
                                               symtab, consttab),
                     newC = apply_symbol_table(header->pattern(),
                                               true,
                                               symtab, consttab),
                     newR = apply_symbol_table(header->rightContext(),
                                               true,
                                               symtab, consttab);
                if ((!newL && header->leftContext().size())
                  ||(!newC && header->pattern().size())
                  ||(!newR && header->rightContext().size())
                ) {
                        std::cout << "internal error: after apply_symbol_table"
                                "(), one of newL, newR, or newC is null"
                                << std::endl;
                        return boost::optional<Production>();
                }

                ProductionHeader ph(*header);
                ph.setLeftContext (*newL);
                ph.setPattern(*newC);
                ph.setRightContext (*newR);

                if (header->condition()) {
                        const optional<Parameter> cond = apply_symbol_table(
                                                *header->condition(),
                                                false,
                                                symtab, consttab);
                        if (!cond) {
                                std::cout <<
                                "internal error: after apply_symbol_table"
                                "(), cond is null"
                                << std::endl;
                                return boost::optional<Production>();
                        }
                        ph.setCondition (*cond);
                }

                ret.setHeader (ph);

                for (unsigned int p=0; p<(*bodies).size(); ++p) {
                        ProductionBody &body = (*bodies)[p];
                        const boost::optional<Pattern> newp =
                                apply_symbol_table(body.pattern(), false,
                                                   symtab, consttab);
                        if (!newp)
                                return boost::optional<Production>();
                        body.setPattern (*newp);
                }
                ret.setBodies (*bodies);
        }
        //---------------------------------------------------------


        behind = it;
        return ret;
}


boost::optional<Constant> parse_constant (TokenIterator it,
                                          TokenIterator end,
                                          TokenIterator &behind
) {
        using boost::optional;
        const TokenIterator startIt = it;
        if (it == end) {
                return optional<Constant>();
        }
        if (it->type() != Token::Identifier) {
                return optional<Constant>();
        }
        Constant c;
        c.setName (it->value());
        ++it;

        if (it == end || it->type() != Token::Equals) {
                return optional<Constant>();
        }

        ++it;
        if (it == end) {
                const TokenIterator prev = it-1;
                std::cerr
                        << "error: expected number after '=' in line "
                        << prev->to().row() << ", column " << prev->to().column()
                        << ".";
                return optional<Constant>();
                return optional<Constant>();
        }

        bool negative = false;
        if (it->type() == Token::Minus) {
                ++it;
                if (it == end
                  || (it->type()!=Token::Real
                     && it->type()!=Token::Integer)
                ) {
                        const TokenIterator prev = it-1;
                        std::cerr
                                << "error: expected number after '-' in line "
                                << prev->to().row()
                                << ", column " << prev->to().column()
                                << ".";
                        return optional<Constant>();
                }
                negative = true;
        }


        if (it->type() == Token::Real) {
                c.setType(Constant::Real);
                c.setReal(negative ?
                          -it->valueAsReal() :
                          it->valueAsReal());
        } else if (it->type() == Token::Integer) {
                c.setType(Constant::Integer);
                c.setInteger(negative?
                             -it->valueAsInteger() :
                             it->valueAsInteger());
        } else if (it->type() == Token::String) {
                c.setType(Constant::String);
                c.setString(it->value());
        }
        ++it;

        if (it == end || it->type() != Token::Semicolon) {
                TokenIterator prev = it - 1;
                std::cerr << "error: expected ';' after constant in line "
                        << prev->to().row() << ", column "
                        << prev->to().column() << std::endl;
                return boost::optional<Constant>();
        }
        behind = ++it;

        return c;
}


void generate_warnings (LSystem const &lsys,
                        Pattern const &pat,
                        std::map<std::string, Segment> &first_appearance
) {
        for (unsigned int i=0; i<pat.size(); ++i) {
                switch (pat[i].type()) {
                case Segment::Letter:
                        if (!first_appearance.count(pat[i].name())) {
                                first_appearance[pat[i].name()] = pat[i];
                        } else if (pat[i] != first_appearance[pat[i].name()]) {
                                Segment const & sym =
                                  first_appearance[pat[i].name()];
                                std::cerr
                                << "warning: letter '"
                                << pat[i].name() << "' used with"
                                << " differing parameter-counts, "
                                << "'" << pat[i] << "' won't match "
                                << "'" << sym << "'" << std::endl;
                        }
                        break;
                case Segment::Branch:
                        generate_warnings(lsys, pat[i].branch(),
                                          first_appearance);
                        break;
                }
        }
}


void generate_warnings (LSystem const &lsys) {
        std::vector<Production> const prods = lsys.productions();

        std::map<std::string, Segment> first_appearance;
        for (unsigned int i=0; i<prods.size(); ++i) {

                // Check if there are ambiguous rules which could lead
                // to a situation like:
                //  axiom: a   b   a
                //      A: a < b
                //      B:     b < a
                if (i<prods.size()-1
                   && ambiguous (prods[i], prods[i+1])
                ) {
                        std::cerr << "warning: productions '"
                          << prods[i].header().name()
                          << "' and '"
                          << prods[i+1].header().name()
                          << "' might be ambiguous (neither is more "
                          << "specialized); in such case, '"
                          << prods[i].header().name()
                          << "' will be preferred (because it was "
                          << "declared first).\n";
                        ;
                }

                // ABoP seems to allow for overloading, i.e. "b" != "b(x)".
                // I guess that can cause confusion, thus we'll try to warn
                // about overloads.
                const Pattern pats[4] = {
                        prods[i].header().leftContext(),
                        prods[i].header().pattern(),
                        prods[i].header().rightContext()
                };
                for (unsigned int p=0; p<4; ++p) {
                        const Pattern &pat = pats[p];
                        generate_warnings(lsys, pat, first_appearance);
                }
                for (unsigned int p=0; p<prods[i].bodies().size(); ++p) {
                        const Pattern &pat = prods[i].bodies()[p].pattern();
                        generate_warnings(lsys, pat, first_appearance);
                }

        }
}

} }

namespace xyto { 

boost::optional<LSystem> compile (const char *code) {
        const TokenVector tokens = tokenize (code);

        std::vector<Production> prods;
        boost::optional<Pattern> axiom;

        std::set<std::string> names;
        std::map<std::string, Constant> constants;
        TokenIterator it = tokens.begin();
        const TokenIterator &end = tokens.end();

        //char c = 'a';
        while (it != end) {
                TokenIterator behind;

                if (boost::optional<Pattern> a = parse_axiom(it, end, behind,
                                                             constants)){
                        if (axiom) {
                                std::cerr<<"error: multiple axioms.\n";
                                return boost::optional<LSystem>();
                        }
                        axiom = *a;
                } else if (boost::optional<Production> op = parse_production (
                                        it, end, behind, constants)
                ) {
                        const std::string &name = op->header().name();
                        if (names.count(name)) {
                                std::cerr << "error: multiple productions are "
                                 << "named '" << name << "'.\n";
                                return boost::optional<LSystem>();
                        } else {
                                names.insert(op->header().name());
                                prods.push_back(*op);
                        }
                } else if (boost::optional<Constant> c = parse_constant(it,end,
                                                                        behind)
                ) {
                        if (constants.count(c->name())) {
                                std::cerr << "error: multiple constants are "
                                 << "named '" << c->name() << "'.\n";
                                return boost::optional<LSystem>();
                        }
                        constants [c->name()] = *c;
                } else {
                        std::cerr << "error: statement beginning at line " <<
                                it->from().row() << ", column " <<
                                it->from().column() << " is neither a "
                                "production, nor a constant-declaration."
                                << std::endl;
                        return boost::optional<LSystem>();
                }
                it = behind;
        }

        if (!axiom) {
                std::cerr << "error: missing axiom.\n";
                return boost::optional<LSystem>();
        }

        std::stable_sort (prods.begin(), prods.end(), hasPrecedenceOver);

        LSystem sys;
        sys.setConstants(constants);
        sys.setAxiom (*axiom);
        sys.setProductions(prods);
        generate_warnings(sys);
        return sys;
                /*std::cout << "axiom: " << *ax << '\n';

                kallisto::random::marsaglia::UNI rng(1,2,3,4);
                rng.skip(1024);
                Pattern pat = *ax;
                for (int step=0; step<5; ++step) {
                        boost::optional<Pattern> apply(
                                std::vector<Production> const &,
                                Pattern const &,
                                kallisto::random::marsaglia::UNI &rng
                        );

                        boost::optional<Pattern> next = apply (prods,
                                                               pat,
                                                               rng);
                        std::cout << "step " << step+1 << ": ";
                        if (next) {
                                pat = *next;
                                std::cout << pat << '\n';
                        } else {
                                std::cout << "<no match>\n";
                                break;
                        }
                }*/
}

}
