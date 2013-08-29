// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef UNITTESTING_HH_20130822
#define UNITTESTING_HH_20130822

#include "AST.hh"
#include "ASTPrinters/PrettyPrinter.hh"
#include "catch.hpp"
#include <stdexcept>

namespace excyrender { namespace Nature { namespace Et1 { namespace detail {

inline std::shared_ptr<AST::Program> to_ast(std::string const &code) {
    auto toks = tokenize (code);
    if (toks.empty())
        throw std::runtime_error("no tokens");

    std::shared_ptr<AST::Program> prog = excyrender::Nature::Et1::AST::program(toks.begin(), toks.end());
    if (!prog) {
        throw std::logic_error("not compilable: " + code);
    }
    return prog;
}

inline
bool equal (std::string const &in, std::string const &expected,
            std::function<void (std::shared_ptr<excyrender::Nature::Et1::AST::Program>)> pass)
{
    using namespace excyrender::Nature::Et1;

    std::shared_ptr<AST::Program> prog = to_ast(in);
    if (!prog) {
        throw std::logic_error("not compilable: " + in);
    }
    std::stringstream ss;
    {
        ASTPrinters::PrettyPrinter dumper(ss);
        pass(prog);
        prog->accept(dumper);
    }
    if (tokenize(ss.str()) != tokenize(expected)) {
        std::clog << "expected: " << tokenize(expected) << '\n'
                  << "got.....: " << tokenize(ss.str()) << std::endl;
        return false;
    }
    return true;
}

} } } }


#endif // UNITTESTING_HH_20130822
