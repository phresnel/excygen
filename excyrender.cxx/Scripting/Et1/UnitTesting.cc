// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef UNITTESTING_HH_20130822
#define UNITTESTING_HH_20130822

#include "UnitTesting.hh"
#include "Backends/PrettyPrinter.hh"
#include <stdexcept>
#include <sstream>

namespace excyrender { namespace Nature { namespace Et1 { namespace detail {

std::shared_ptr<AST::Program> to_ast(std::string const &code)
{
    auto toks = tokenize (code);
    if (toks.empty())
        throw std::runtime_error("no tokens");

    std::shared_ptr<AST::Program> prog;
    try {
        prog = excyrender::Nature::Et1::AST::program(toks.begin(), toks.end());
        if (!prog) {
            throw std::logic_error("not compilable: " + code);
        }
    } catch (std::exception &e) {
        throw std::logic_error(std::string("in to_ast(), upon building ast: ") + e.what());
    }
    if (toks.end() != prog->to())
        throw std::logic_error("in to_ast(), '" + code + "' was not parsed completely");
    return prog;
}

bool equal (std::string const &in, std::string const &expected,
            std::function<void (std::shared_ptr<excyrender::Nature::Et1::AST::Program>)> pass)
{

    using namespace excyrender::Nature::Et1;

    std::shared_ptr<AST::Program> prog;

    try {
        prog = to_ast(in);
        if (!prog) {
            throw std::logic_error("not compilable: " + in);
        }
    } catch (std::exception &e) {
        throw std::logic_error(std::string("in equal(), upon building ast: ") + e.what());
    }

    std::string got;
    try {
        std::stringstream ss;
        ASTPrinters::PrettyPrinter dumper(ss);
        pass(prog);
        prog->accept(dumper);
        got = ss.str();
    } catch (std::exception &e) {
        throw std::logic_error(std::string("in equal(), upon transform and pretty printing: ") + e.what());
    }
    try {
        const auto got_toks = tokenize(got),
                   expected_toks = tokenize(expected);
        if (got_toks != expected_toks) {
            std::clog << "--tokens-------------------------------------------\n"
                      << "got.....: " << got_toks << '\n'
                      << "expected: " << expected_toks << '\n'
                      << "--pretty-------------------------------------------\n"
                      << "got{\n" << got << "\n}\n"
                      << "expected{\n" << ASTPrinters::pretty_print(expected) << "\n}\n"
                      << "---------------------------------------------------\n";
            return false;
        }
    } catch (std::exception &e) {
        throw std::logic_error(std::string("in equal(), upon tokenization: ") + e.what());
    }
    return true;
}

} } } }


#endif // UNITTESTING_HH_20130822
