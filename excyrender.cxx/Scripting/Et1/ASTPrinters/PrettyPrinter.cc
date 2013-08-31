// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "PrettyPrinter.hh"


//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"

bool compare_token_sequence (std::string const &code) {
    using namespace excyrender::Nature::Et1;
    std::stringstream ss;
    auto tokens = tokenize(code);
    ASTPrinters::PrettyPrinter pp(ss);
    auto ast = AST::program(tokens.begin(), tokens.end());
    if (!ast) {
        throw std::logic_error("not a compilable program: [" + code + "]");
    }
    ast->accept(pp);
    auto result = tokenize(ss.str()) == tokens;
    if (!result) {
        std::cout << "tokens: (\n " << tokenize(ss.str()) << "\n" << " " << tokenize(code) << "\n)\n";
        std::cout << "input: {\n" << code << "\n}\n";
        std::cout << "pretty: {\n" << ss.str() << "\n}\n";
    }
    return result;
}

TEST_CASE( "Et1/ASTPrinters/PrettyPrinter", "Pretty printing" ) {
    REQUIRE(compare_token_sequence("let f(x) = x*2 in f(1)"));
    REQUIRE(compare_token_sequence("let f(x) = x+1+2 in f(3)"));
    REQUIRE(compare_token_sequence("let f(x,y,z) = x+y+z*f(x/y,2+z,-1-z) in f(1,2,3)"));
    REQUIRE(compare_token_sequence("let int f(x) = x*2 in f(1)"));
}
//--------------------------------------------------------------------------------------------------

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPrinters {

std::string pretty_print(std::string in) {
    auto ast = detail::to_ast(in);
    if (!ast) return "";
    std::stringstream ss;
    PrettyPrinter pp(ss);
    ast->accept(pp);
    return ss.str();
}

} } } }
