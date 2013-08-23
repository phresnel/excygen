// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "find_references.hh"


//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"

static bool test(std::string const &code, std::set<std::string> expected, bool cross_bindings) {
    using namespace excyrender::Nature::Et1;

    auto tokens = tokenize(code);
    auto ast    = AST::program(tokens.begin(), tokens.end());
    auto names  = ASTQueries::find_references(ast, cross_bindings);

    return names == expected;
}

TEST_CASE( "Et1/ASTQueries/find_references", "Reference finding" ) {

    using namespace excyrender::Nature::Et1;

    // Simple check.
    REQUIRE(test("let f = 1, "
                 "    g = 2, "
                 "    h = 3  "
                 "in f+g+h   ",
                 {"f","g","h"},
                 false));

    // Test that function callees are not found.
    REQUIRE(test("let f(x) = 1, "
                 "    g = 2, "
                 "    h = 3  "
                 "in f(g)+g+h   ",
                 {"g","h"},
                 false));

    // Finding *with* crossing binding-boundaries.
    REQUIRE(test("let f = 1, "
                 "    g = let i = 2 in let j = 4 in j, "
                 "    h = 3  "
                 "in f+g+h   ",
                 {"f","g","h", "j"},
                 true));

    // Finding *without* crossing binding-boundaries.
    REQUIRE(test("let f = 1, "
                 "    g = let i = 2 in let j = 4 in j, "
                 "    h = 3  "
                 "in f+g+h   ",
                 {"f","g","h"},
                 false));
}
//--------------------------------------------------------------------------------------------------




namespace excyrender { namespace Nature { namespace Et1 { namespace ASTQueries {

namespace {

    struct FindRefs final : AST::Visitor {
        FindRefs (std::set<std::string> &refs, bool cross_bindings) :
            refs(&refs), cross_bindings(cross_bindings) {}

        void begin(AST::Addition const &) {}
        void end(AST::Addition const &) {}

        void begin(AST::Subtraction const &) {}
        void end(AST::Subtraction const &) {}

        void begin(AST::Multiplication const &) {}
        void end(AST::Multiplication const &) {}

        void begin(AST::Division const &) {}
        void end(AST::Division const &) {}

        void begin(AST::IntegerLiteral const &) {}
        void end(AST::IntegerLiteral const &) {}

        void begin(AST::Call const &) {}
        void end(AST::Call const &) {}

        void begin(AST::Negation const &) {}
        void end(AST::Negation const &) {}

        void begin(AST::ParenExpression const &) {}
        void end(AST::ParenExpression const &) {}

        void begin(AST::Binding const &)
        {
            ++binding_depth;
        }
        void end(AST::Binding const &)
        {
            --binding_depth;
        }

        void begin(AST::Identifier const &id)
        {
            if (!cross_bindings && binding_depth>0)
                return;
            refs->insert(id.id());
        }
        void end(AST::Identifier const &) {}

        void begin(AST::LetIn const &) {}
        void end(AST::LetIn const &) {}

        void begin(AST::Program const &) {}
        void end(AST::Program const &) {}

    private:
        std::set<std::string> *refs;
        bool cross_bindings;
        int binding_depth = 0;
    };

}


std::set<std::string> find_references (shared_ptr<AST::ASTNode> ast, bool cross_bindings) {
    std::set<std::string> ret;
    FindRefs fr(ret, cross_bindings);
    ast->accept(fr);
    return ret;
}


std::set<std::string> find_references (AST::ASTNode const &ast, bool cross_bindings) {
    std::set<std::string> ret;
    FindRefs fr(ret, cross_bindings);
    ast.accept(fr);
    return ret;
}


} } } }
