// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "1100_resolve_types.hh"

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

    REQUIRE(equal("let f(x) = 1 in f(2)",
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

    class TryResolve final : public Visitor {

        stack<string> scope;

        void reduce_binary(string op) {
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (1)");
            const string rhs = scope.top();
            scope.pop();
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (2)");
            const string lhs = scope.top();
            scope.pop();

            if (lhs == rhs) {
                scope.push(lhs);
            } else {
                scope.push("<" + lhs + op + rhs + ">");
            }
        }

    public:
        void begin(Addition const &) {}
        void end(Addition const &) { reduce_binary("+"); }

        void begin(Subtraction const &) {}
        void end(Subtraction const &) { reduce_binary("-"); }

        void begin(Multiplication const &) {}
        void end(Multiplication const &) { reduce_binary("*"); }

        void begin(Division const &) {}
        void end(Division const &) { reduce_binary("/"); }

        void infix() {}

        void begin(IntegerLiteral const &) { scope.push("int"); }
        void end(IntegerLiteral const &) {}

        void begin(Call const &) { scope.push("<call>"); }
        void end(Call const &) {}

        void begin(Negation const &) {}
        void end(Negation const &) {}

        void begin(ParenExpression const &) { scope.push("<paren-expr>"); }
        void end(ParenExpression const &) {}

        void begin(Binding const &) {}
        void end(Binding const &) {}

        void begin(AST::Identifier const &) { scope.push("<id>"); }
        void end(AST::Identifier const &) {}

        void begin(LetIn const &) {}
        void before_body(LetIn const &) {}
        void end(LetIn const &) {}

        void begin(Program const &) {}
        void end(Program const &) {}

        std::string type() const {
            if (scope.empty()) {
                throw std::logic_error("TryResolve type stack empty when type() is called");
            }
            if (scope.size()>1) {
                throw std::logic_error("TryResolve type stack not fully reduced when type() is called");
            }
            return scope.top();
        }

    };

    std::string try_resolve (ASTNode const &ast) {
        TryResolve tr;
        ast.accept (tr);
        return tr.type();
    }

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

        void begin(Call &) {}
        void end(Call &) {}

        void begin(Negation &) {}
        void end(Negation &) {}

        void begin(ParenExpression &) {}
        void end(ParenExpression &) {}

        void begin(Binding &binding)
        {
            std::cout << "???" << binding.id() << " <-- " << try_resolve(binding.body()) << std::endl;
        }
        void end(Binding &)
        {
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
