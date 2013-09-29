// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "1200_globalize_functions.hh"
#include "1150_mangle.hh"
#include "1100_resolve_types.hh"
#include "1000_lambda_lift.hh"

#include "../ASTAlgorithm.hh"

//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../Backends/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTPasses/1200_globalize_functions", "Globalize functions" ) {
    using std::string;
    using namespace excyrender::Nature::Et1;
    using namespace excyrender::Nature::Et1::ASTPrinters;
    using namespace excyrender::Nature::Et1::ASTPasses;
    using namespace excyrender::Nature::Et1::detail;

    auto passes = [](std::shared_ptr<AST::Program> ast) { lambda_lift(ast);
                                                          resolve_types(ast);
                                                          mangle(ast);
                                                          globalize_functions(ast); };
    return;

    // Non nested.
    REQUIRE(equal("let y = true in y",
                  "let bool y = true in y",
                  passes));

    REQUIRE(equal("let f(x) = true in f(2)",

                  "program bool $f$_$auto$(x) = true, "
                  "        bool $f$_$int$(int x) = true "
                  " in $f$_$int$(2)",
                  passes));

    REQUIRE(equal("let f(x) = x, "
                  "    y = f(2.0) "
                  "in f(2)",

                  "program $f$_$auto$(x) = x, "
                  "    float $f$_$float$(float x) = x, "
                  "    int $f$_$int$(int x) = x "
                  "in let float y = $f$_$float$(2.0) "
                  "in $f$_$int$(2)",
                  passes));
}
//- Tests ------------------------------------------------------------------------------------------


namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses { namespace {

    struct GlobalizeFunctions : AST::Transform {
        void begin(AST::Addition &) {}
        void end(AST::Addition &) {}
        void begin(AST::Subtraction &) {}
        void end(AST::Subtraction &) {}
        void begin(AST::Multiplication &) {}
        void end(AST::Multiplication &) {}
        void begin(AST::Division &) {}
        void end(AST::Division &) {}

        void begin(AST::LessThan &) {}
        void end(AST::LessThan &) {}
        void begin(AST::LessEqual &) {}
        void end(AST::LessEqual &) {}
        void begin(AST::GreaterThan &) {}
        void end(AST::GreaterThan &) {}
        void begin(AST::GreaterEqual &) {}
        void end(AST::GreaterEqual &) {}
        void begin(AST::Equal &) {}
        void end(AST::Equal &) {}
        void begin(AST::NotEqual &) {}
        void end(AST::NotEqual &) {}
        void begin(AST::LogicalAnd &) {}
        void end(AST::LogicalAnd &) {}
        void begin(AST::LogicalOr &) {}
        void end(AST::LogicalOr &) {}
        void begin(AST::LogicalNot &) {}
        void end(AST::LogicalNot &) {}

        void transform(AST::IntegerLiteral &) {}
        void transform(AST::RealLiteral &) {}
        void transform(AST::BoolLiteral &) {}
        void transform(AST::Identifier &) {}

        void begin(AST::Call &) {}
        void end(AST::Call &) {}

        void begin(AST::Negation &) {}
        void end(AST::Negation &) {}

        void begin(AST::ParenExpression &) {}
        void end(AST::ParenExpression &) {}

        void begin(AST::Binding &) {}
        void end(AST::Binding &) {}

        void begin(AST::IfThenElse &) {}
        void end(AST::IfThenElse &) {}

        void begin(AST::LetIn &letin) {
            for (auto &b : letin.bindings()) {
                //if (b->kind() == AST::Binding::Function)
                    bindings_->push_back(b);
            }
        }
        void end(AST::LetIn &) {}

        void begin(AST::Program &prog) {
            bindings_ = &prog.bindings();
        }
        void end(AST::Program &) {}

    private:
        vector<shared_ptr<AST::Binding>> *bindings_ = nullptr;
    };
} } } } }


namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

void globalize_functions(shared_ptr<AST::ASTNode> ast)
{
    // Copy all functions to the global namespace.
    GlobalizeFunctions gf;
    ast->accept(gf);

    // Remove local function definitions.
    Algorithm::transform_expressions(*ast, [] (shared_ptr<AST::Expression> &expr) {
        AST::LetIn* letin = dynamic_cast<AST::LetIn*>(expr.get());
        if (!letin) return;

        // remove all function-bindings
        auto &bindings = letin->bindings();
        /*auto from = std::remove_if(bindings.begin(), bindings.end(),
                             [] (shared_ptr<AST::Binding> b) {
                                  return b->kind() == AST::Binding::Function; } );
        bindings.erase(from, bindings.end());*/
        bindings.clear();

        // replace all LetIns without bindings by just their value.
        if (letin->bindings().empty()) {
            expr = letin->value_ptr();
        }
    });


    // TODO: extract the transform-algorithm
    // TODO: write an unused-elimination pass at phase 1150
}

} } } }
