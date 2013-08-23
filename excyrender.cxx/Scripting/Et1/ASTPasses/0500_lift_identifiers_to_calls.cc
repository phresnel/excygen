// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "0500_lift_identifiers_to_calls.hh"
#include "1000_lambda_lift.hh"



//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../ASTPrinters/PrettyPrinter.hh"

TEST_CASE( "Et1/Passes/0500_lift_identifiers_to_calls", "Identifier to call promotion" ) {
    using namespace excyrender::Nature::Et1;
    using detail::equal;

    auto passes = [](std::shared_ptr<AST::Program> ast) { ASTPasses::lambda_lift(ast); };

    REQUIRE(equal("let x = "
                  "   let y = x*2 "
                  "   in y "
                  "in x",
                  "let x = "
                  "   let y(x) = x*2 "
                  "   in y(x) "
                  "in x ",
                  passes));

    REQUIRE(equal("let x = "
                  "   let a = "
                  "      let b = "
                  "         let c = "
                  "             let d = x "
                  "             in d "
                  "         in c "
                  "      in b "
                  "   in a "
                  "in x ",
                  "let x = "
                  "   let a(x) = "
                  "      let b(x) = "
                  "         let c(x) = "
                  "             let d(x) = x "
                  "             in d(x) "
                  "         in c(x) "
                  "      in b(x) "
                  "   in a(x) "
                  "in x ",
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
    struct LiftIdentifiersToCalls final : Transform {
    private:
        bool shall(std::string const &id) const {
            return which.end() != which.find(id);
        }

        void binary(Binary &op) {
            if (auto id = dynamic_cast<AST::Identifier*>(&op.lhs()))
                if (shall (id->id())) op.reset_lhs(new Call(id->from(), id->to(), id->id(), {}));
            if (auto id = dynamic_cast<AST::Identifier*>(&op.rhs()))
                if (shall (id->id())) op.reset_rhs(new Call(id->from(), id->to(), id->id(), {}));
        }

    public:
        LiftIdentifiersToCalls (std::set<std::string> const &which,
                                bool cross_bindings) :
            which(which), cross_bindings(cross_bindings)
        {}

        void begin(Addition &op) { binary(op); }
        void end(Addition &) {}

        void begin(Subtraction &op) { binary(op); }
        void end(Subtraction &) {}

        void begin(Multiplication &op) { binary(op); }
        void end(Multiplication &) {}

        void begin(Division &op) { binary(op); }
        void end(Division &) {}

        void begin(IntegerLiteral &) {}
        void end(IntegerLiteral &) {}

        void begin(Call &call) {
            if (!cross_bindings && binding_depth>0)
                return;
            for (auto &a : call.arguments()) {
                if (auto id = dynamic_cast<AST::Identifier*>(&*a))
                    if (shall (id->id()))a.reset(new Call(id->from(), id->to(), id->id(), {}));
            }
        }
        void end(Call &) {}

        void begin(Negation &n) {
            if (!cross_bindings && binding_depth>0)
                return;
            if (auto id = dynamic_cast<AST::Identifier*>(&n.rhs()))
                if (shall (id->id()))n.reset_rhs(new Call(id->from(), id->to(), id->id(), {}));
        }
        void end(Negation &) {}

        void begin(ParenExpression &) {}
        void end(ParenExpression &) {}

        void begin(Binding &binding) {
            ++binding_depth;
            if (cross_bindings || binding_depth==0) {
                if (auto id = dynamic_cast<AST::Identifier*>(&binding.body()))
                    if (shall (id->id()))binding.reset_body(new Call(id->from(), id->to(), id->id(), {}));
            }
        }
        void end(Binding &) {
            --binding_depth;
        }

        void begin(AST::Identifier &) {}
        void end(AST::Identifier &) {}

        void begin(LetIn &letin) {
            if (!cross_bindings && binding_depth>0)
                return;
            if (auto id = dynamic_cast<AST::Identifier*>(&letin.value()))
                if (shall (id->id()))letin.reset_value(new Call(id->from(), id->to(), id->id(), {}));
        }
        void end(LetIn &) {}

        void begin(Program &prog) {
            if (!cross_bindings && binding_depth>0)
                return;
            if (auto id = dynamic_cast<AST::Identifier*>(&prog.value()))
                if (shall (id->id()))prog.reset_value(new Call(id->from(), id->to(), id->id(), {}));
        }
        void end(Program &) {}

    private:
        std::set<std::string> which;
        bool cross_bindings;
        int binding_depth = 0;
    };
}

void lift_identifiers_to_calls(shared_ptr<AST::ASTNode> ast, std::set<std::string> const &which, bool cross_bindings) {
    LiftIdentifiersToCalls l(which, cross_bindings);
    ast->accept(l);
}

void lift_identifiers_to_calls(AST::ASTNode &ast, std::set<std::string> const &which, bool cross_bindings) {
    LiftIdentifiersToCalls l(which, cross_bindings);
    ast.accept(l);
}


} } } }
