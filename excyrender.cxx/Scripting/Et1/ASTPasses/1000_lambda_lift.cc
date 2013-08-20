// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "1000_lambda_lift.hh"
#include "../ASTQueries/find_references.hh"
#include "../ASTQueries/find_binding_names.hh"

#include <stack>
#include <iostream>
#include <stdexcept>
#include <algorithm>


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

        void begin(IntegerLiteral &) {}
        void end(IntegerLiteral &) {}

        void begin(Call &call)
        {
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

        void begin(AST::Identifier &) {}
        void end(AST::Identifier &) {}

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
                binding.arguments().push_back(Argument("$lifted any", a));
            }


            // Inject new argument at all call sites.
            if (auto parent = scope.top().parent) {
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

        void begin(AST::Identifier &) {}
        void end(AST::Identifier &) {}

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
    while (1) {
        std::cout << "lambda-lift ...\n";
        LambdaLift ll;
        ast->accept(ll);
        if (!ll.transformed())
            break;
    }
}


} } } }
