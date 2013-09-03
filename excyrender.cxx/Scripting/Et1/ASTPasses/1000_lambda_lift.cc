// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "0500_lift_identifiers_to_calls.hh"
#include "1000_lambda_lift.hh"
#include "../ASTQueries/find_references.hh"
#include "../ASTQueries/find_binding_names.hh"

#include <stack>
#include <iostream>
#include <stdexcept>
#include <algorithm>



//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../Backends/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTPasses/1000_lambda_lift", "Lambda Lifting" ) {
    using namespace excyrender::Nature::Et1;
    using detail::equal;

    auto passes = [](std::shared_ptr<AST::Program> ast) { ASTPasses::lambda_lift(ast); };

    REQUIRE(equal("1 + 2", "1 + 2", passes));

    REQUIRE(equal("let f(x) = let z=x in z in f(1)",
                  "let f(x) = let z(x) = x in z(x) in f(1)",
                  passes));

    // The following also tests that within P(), z shall not be replaced.
    REQUIRE(equal("let f(x) = "
                  "  let g(y) = let z=x*2, "
                  "                 P(z)=z "
                  "             in z "
                  "  in 2+g(-(1+g(42))) "
                  "in f(1) ",

                  "let f(x) = "
                  "  let g(y,x) = let z(x)=x*2, "
                  "                   P(z)=z "
                  "               in z(x) "
                  "  in 2+g(-(1+g(42,x)),x) "
                  "in f(1) ",
                  passes));
}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

namespace {

    using namespace AST;
    using std::set;

    struct BindingScope {
        Binding &binding;
        Binding *parent;

        BindingScope(Binding &binding, Binding *parent) :
            binding(binding), parent(parent) {}
    };


    struct AppendLifted final : Transform {

        AppendLifted(string call_name, set<string> const &lifted_args) :
            call_name(call_name), lifted_args(lifted_args)
        {}

        void begin(Addition &) {}
        void end(Addition &) {}
        void begin(Subtraction &) {}
        void end(Subtraction &) {}
        void begin(Multiplication &) {}
        void end(Multiplication &) {}
        void begin(Division &) {}
        void end(Division &) {}

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
        void transform(AST::Identifier &id)
        {
            if (binding_depth != 1)
                return;
            if (id.id() != call_name)
                return;
            throw std::logic_error("AppendLifted::begin(Identifier): reached point deemed unreachable");
        }

        void begin(Call &call)
        {
            if (binding_depth != 1)
                return;
            if (call.id() != call_name)
                return;
            for (auto a : lifted_args) {
                call.arguments().push_back(shared_ptr<AST::Identifier>(
                        new AST::Identifier(call.from(), call.to(), a)));
            }
        }
        void end(Call &) {}

        void begin(Negation &) {}
        void end(Negation &) {}

        void begin(ParenExpression &) {}
        void end(ParenExpression &) {}

        void begin(Binding &binding)
        {
            ++binding_depth;
        }
        void end(Binding &)
        {
            --binding_depth;
        }

        void begin(IfThenElse &) {}
        void end(IfThenElse &) {}

        void begin(LetIn &) {}
        void end(LetIn &) {}

        void begin(Program &) {}
        void end(Program &) {}

    private:
        int binding_depth = 0;
        string call_name;
        set<string> lifted_args;
    };

    struct LambdaLift final : Transform {
        bool transformed() const { return transformed_; }

        void begin(Addition &) {}
        void end(Addition &) {}
        void begin(Subtraction &) {}
        void end(Subtraction &) {}
        void begin(Multiplication &) {}
        void end(Multiplication &) {}
        void begin(Division &) {}
        void end(Division &) {}

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
        void transform(AST::Identifier &id) {}

        void begin(Call &) {}
        void end(Call &) {}

        void begin(Negation &) {}
        void end(Negation &) {}

        void begin(ParenExpression &) {}
        void end(ParenExpression &) {}

        void begin(Binding &binding)
        {
            scope.push(BindingScope(binding,
                                    scope.empty() ? nullptr
                                                  : &scope.top().binding));

            // Find all references
            // * for which there is no argument in this binding,
            // * which don't refer to the binding itself,
            // * which don't refer to one of their own bindings.
            auto non_locals = ASTQueries::find_references(binding.body(), false);

            for (Argument a : binding.arguments())
                non_locals.erase(a.name);
            non_locals.erase(binding.id());

            auto bodies_own_names = ASTQueries::find_binding_names(binding.body(), false);
            for (auto a : bodies_own_names)
                non_locals.erase(a);

            if (non_locals.empty())
                return;

            // Append all non-local references onto the argument list.
            for (auto a : non_locals) {
                binding.arguments().push_back(Argument(AST::Typeinfo(), a));
            }


            // Inject new argument at all call sites.
            if (auto parent = scope.top().parent) {
                // There might be identifiers within the parent's body which must now be promoted
                // to calls.
                lift_identifiers_to_calls(parent->body(), {binding.id()}, false);

                // Append the lifted variable-names to all calls of us.
                AppendLifted al(binding.id(), non_locals);
                parent->accept(al);
            } else {
                //throw std::runtime_error("unresolved identifiers (lambda lifting: unliftable)");
            }

            transformed_ = true;
        }
        void end(Binding &)
        {
            scope.pop();
        }

        void begin(IfThenElse &) {}
        void end(IfThenElse &) {}

        void begin(LetIn &) {}
        void end(LetIn &) {}

        void begin(Program &) {}
        void end(Program &) {}
    private:
         std::stack<BindingScope> scope;
         bool transformed_ = false;
    };

}

void lambda_lift(shared_ptr<AST::ASTNode> ast) {
    if (!ast) {
        //std::clog << "pass: lambda-lifting skipped, AST is empty\n";
        return;
    }
    int i = 0;
    while (1) {
        ++i;
        LambdaLift ll;
        ast->accept(ll);
        if (!ll.transformed())
            break;
    }
    std::clog << "pass: lambda-lifting (" << i << "x)\n";
}


} } } }
