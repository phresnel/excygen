// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "1100_resolve_types.hh"
#include "../ASTQueries/resolve_type.hh"

#include <stack>
#include <iostream>
#include <stdexcept>
#include <algorithm>



//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../ASTPrinters/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTPasses/1100_resolve_types.hh", "Type resolution" ) {

    using namespace excyrender::Nature::Et1;
    using detail::equal;

    auto passes = [](std::shared_ptr<AST::Program> ast) { ASTPasses::resolve_types(ast); };

    /*REQUIRE(equal("let f(x) = y in f(2)",  // TODO: make this fail
                  "let int f(int x) = 1 in f(2)",
                  passes));*/

    REQUIRE(equal("let f(int x) = 1 in f(2)",
                  "let int f(int x) = 1 in f(2)",
                  passes));
return;
    REQUIRE(equal("let f(x) = x in f(2)",
                  "let int f(int x) = 1 in f(2)",
                  passes));

    REQUIRE(equal("let f(x) = 1.0 in f(2)",
                  "let float f(int x) = 1.0 in f(2)",
                  passes));

    REQUIRE(equal("let f(x) = 1.0 in f(2)",
                  "let float f(float x) = 1.0 in f(2.0)",
                  passes));

    // This tests auto-emitted overloads.
    REQUIRE(equal("let f(x) = 1, "
                  "    a = f(1), "
                  "    b = f(1.0) "
                  "in 1",

                  "let int f(int x) = 1, "
                  "    int f(float x) = 1, "
                  "    int a = f(1), "
                  "    int b = f(1.0) "
                  "in 1",
                  passes));

    REQUIRE(equal("let f(x,y) = 1, "
                  "    a = f(1, 1),  "
                  "    b = f(1.0, 1), "
                  "    c = f(1, 1.0), "
                  "    d = f(1.0, 1.0) "
                  "in 1",

                  "let int   f(int x, int y) = x, "
                  "    float f(float x, int y) = x, "
                  "    int   f(int x, float y) = x, "
                  "    float f(float x, float y) = x, "
                  "    int   a = f(1, 1),  "
                  "    float b = f(1.0, 1), "
                  "    int   c = f(1, 1.0), "
                  "    float d = f(1.0, 1.0) "
                  "in 1",
                  passes));

    // TODO: check for thrown exceptions upon type mismatches
}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

namespace {
    using namespace AST;
    using std::set;
    using std::stack;
    using std::string;

    struct ResolveTypes final : Transform {
        bool transformed() const { return transformed_; }
        bool has_unresolved() const { return has_unresolved_; }

        void begin(Addition &) {}
        void end(Addition &) {}

        void begin(Subtraction &) {}
        void end(Subtraction &) {}

        void begin(Multiplication &) {}
        void end(Multiplication &) {}

        void begin(Division &) {}
        void end(Division &) {}

        void begin(IntegerLiteral &) {}
        void end(IntegerLiteral &) {}

        void begin(RealLiteral &) {}
        void end(RealLiteral &) {}

        void begin(Call &) {}
        void end(Call &) {}

        void begin(Negation &) {}
        void end(Negation &) {}

        void begin(ParenExpression &) {}
        void end(ParenExpression &) {}

        void begin(Binding &)
        {
            //std::cout << "???" << binding.id() << " <-- " << ASTQueries::resolve_type(binding.body()) << std::endl;
        }
        void end(Binding &binding)
        {
            binding.reset_type(ASTQueries::resolve_type(binding.body()));
        }

        void begin(AST::Identifier &) {}
        void end(AST::Identifier &) {}

        void begin(LetIn &) {}
        void end(LetIn &) {}

        void begin(Program &) {}
        void end(Program &) {}

    private:
         bool transformed_ = false;
         bool has_unresolved_ = false;
    };
}

void resolve_types(shared_ptr<AST::ASTNode> ast) {
    if (!ast) {
        //std::clog << "pass: lambda-lifting skipped, AST is empty\n";
        return;
    }

    int i = 0;
    while (1) {
        ++i;
        ResolveTypes ll;
        ast->accept(ll);
        if (!ll.transformed()) {
            if (ll.has_unresolved()) {
                throw std::runtime_error("program is not fully resolvable");
            }
            break;
        }
    }
    std::clog << "pass: resolve-types (" << i << "x)\n";
}


} } } }
