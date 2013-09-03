// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "../Backends/PrettyPrinter.hh"
#include "equal.hh"


//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"

TEST_CASE( "Et1/ASTQueries/equal", "Value equivalence" ) {

    using namespace excyrender::Nature::Et1::ASTQueries;

    REQUIRE(equal("x", "x"));
    REQUIRE(!equal("y", "x"));
    REQUIRE(equal("let x = 1 in x", "    let\nx\n=\n1\nin\nx\t\t"));
}
//--------------------------------------------------------------------------------------------------

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTQueries {

bool equal(AST::ASTNode const &lhs, AST::ASTNode const &rhs)
{
    return ASTPrinters::pretty_print(lhs) == ASTPrinters::pretty_print(rhs);
}

bool equal(string const &lhs, string const &rhs)
{
    return ASTPrinters::pretty_print(lhs) == ASTPrinters::pretty_print(rhs);
}

} } } }
