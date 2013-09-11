// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "0500_lift_identifiers_to_calls.hh"
#include "0900_unnest_letins.hh"
#include "1000_lambda_lift.hh"
#include "1200_globalize_functions.hh"
#include "../ASTAlgorithm.hh"
#include "../ASTQueries/find_references.hh"

#include <stack>
#include <stdexcept>


//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../Backends/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTPasses/0900_unnest_letins.hh", "Let-In Unnesting" ) {

    using namespace excyrender;
    using namespace excyrender::Nature::Et1;
    using detail::equal;

    auto passes = [](std::shared_ptr<AST::Program> ast) {
        //ASTPasses::unnest_letins(ast);
        ASTPasses::lambda_lift(ast);
        ASTPasses::globalize_functions(ast);
    };

    string str = "let y = true in y";
    auto ast = detail::to_ast(str);

    std::cerr << "-incoming----------------------------------------------------\n";
    std::cerr << ASTPrinters::pretty_print(*ast) << std::endl;

    std::cerr << "-ALPHA-------------------------------------------------------\n";

    int idc = 0;
    auto gen_name = [&idc] () { return "$" + std::to_string(idc++); };

    Algorithm::transform_expressions(*ast, [&gen_name](shared_ptr<AST::Expression> &e) {
        auto letin = dynamic_pointer_cast<AST::LetIn>(e);
        if (letin) {
            std::cerr << "turning to function: " << ASTPrinters::pretty_print(letin) << '\n';

            shared_ptr<AST::Binding> b (new AST::Binding(
                letin->from(), letin->to(),
                gen_name(), letin->type(),
                std::vector<AST::Argument>(),
                letin,
                AST::Binding::Function
            ));

            std::cerr << "new function: " << ASTPrinters::pretty_print(b) << '\n';
        }
    });


    std::cerr << "-END---------------------------------------------------------\n";


    /*REQUIRE(equal("let y = true in y",
                  "let y = true in y",
                  passes));*/

    /*REQUIRE(equal("let x() = 1 in x()",

                  "let $unnested0$y = true, "
                  "    x = $unnested0$y "
                  "in x ",
                  passes));*/
}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

namespace {
    using namespace AST;
    using std::stack;
    using std::string;

    struct UnnestLetins final : Transform {

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

        void transform(IntegerLiteral &) {}
        void transform(RealLiteral &) {}
        void transform(BoolLiteral &) {}

        void transform(AST::Identifier &) {}

        void begin(AST::Call &) {}
        void end(AST::Call &) {}

        void begin(AST::IfThenElse &) {}
        void end(AST::IfThenElse &) {}

        void begin(AST::Negation &) {}
        void end(AST::Negation &) {}

        void begin(AST::ParenExpression &) {}
        void end(AST::ParenExpression &) {}

        void begin(AST::Binding &)
        {
        }
        void end(AST::Binding &binding) {
        }

        void begin(AST::LetIn &letin) {}

        void end(AST::LetIn &letin) {}

        void begin(AST::Program &) {}
        void end(AST::Program &prog) {}

    private:

         int idCounter = 0;
         string auto_id() {
             return "$auto_" + std::to_string(idCounter++) + "$";
         }
    };
}


void unnest_letins(shared_ptr<AST::ASTNode> ast) {
    if (!ast) return;
    UnnestLetins ll;
    ast->accept(ll);
}


} } } }
