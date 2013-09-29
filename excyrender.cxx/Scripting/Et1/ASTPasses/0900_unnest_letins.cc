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

    return;

    using namespace excyrender;
    using namespace excyrender::Nature::Et1;
    using detail::equal;
    using Algorithm::transform_expressions;

    auto passes = [](std::shared_ptr<AST::Program> ast) {
        ASTPasses::unnest_letins(ast);
        ASTPasses::lambda_lift(ast);
        ASTPasses::globalize_functions(ast);
    };

    string str = "let B(y) = if y then 1 else (let x=1 in x)"
                 "in B(true)";
    auto ast = detail::to_ast(str);

    std::cerr << "-incoming----------------------------------------------------\n";
    std::cerr << ASTPrinters::pretty_print(*ast) << std::endl;

    std::cerr << "-ALPHA-------------------------------------------------------\n";
    int idc = 0;
    auto gen_name = [&idc] () { return "$" + std::to_string(idc++); };

    Algorithm::for_each_at_end<AST::LetIn>(*ast, [&gen_name] (AST::LetIn &letin) {

        vector<shared_ptr<AST::Binding>> new_bindings;

        for (auto &binding_ : letin.bindings()) {
            AST::Binding &binding = *binding_;
        //Algorithm::for_each<AST::Binding>(letin, [&] (AST::Binding &binding) {

            transform_expressions(binding, [&binding, &gen_name, &new_bindings](shared_ptr<AST::Expression> &e) {
                auto letin = dynamic_pointer_cast<AST::LetIn>(e);
                if (letin) {
                    //std::cerr << "turning to function: " << ASTPrinters::pretty_print(letin) << '\n';

                    auto args = binding.kind() == AST::Binding::Function
                                ? binding.arguments()
                                : std::vector<AST::Argument>();

                    shared_ptr<AST::Binding> b (new AST::Binding(
                        letin->from(), letin->to(),
                        gen_name(), letin->type(),
                        args,
                        letin,
                        AST::Binding::Function
                    ));
                    new_bindings.push_back(b);
                    //std::cerr << "new function: " << ASTPrinters::pretty_print(b) << '\n';
                    vector<shared_ptr<AST::Expression>> forwarded_args;
                    for (auto a : args) {
                        forwarded_args.emplace_back(new AST::Identifier(letin->from(), letin->to(),
                                                                        a.name));
                    }

                    shared_ptr<AST::Call> call (new AST::Call(b->from(), b->to(),
                                                b->id(), forwarded_args));
                    call->reset_referee(b);
                    e = call;

                    //std::cerr << "new call: " << ASTPrinters::pretty_print(e) << '\n';
                }
            });
        }

        for (auto b : new_bindings) {
            letin.bindings().push_back(b);
        }
    });


    std::cerr << "-outgoing----------------------------------------------------\n";
    ASTPasses::lambda_lift(ast);
    ASTPasses::globalize_functions(ast);
    std::cerr << ASTPrinters::pretty_print(*ast) << std::endl;

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
