// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "1200_globalize_functions.hh"


//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../Backends/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTPasses/1200_globalize_functions", "Globalize functions" ) {
    using std::string;
    using namespace excyrender::Nature::Et1;
    using namespace excyrender::Nature::Et1::ASTPrinters;
    using namespace excyrender::Nature::Et1::ASTPasses;


    string in = "let f(x) = let g(x) = x*let y=2 in y in g(x) in f(x)";
    auto ast = detail::to_ast(in);

    std::cerr << "------------------------\n";
    std::cerr << pretty_print(*ast) << '\n';
    std::cerr << "------------------------\n";
    globalize_functions(ast);
    std::cerr << pretty_print(*ast) << '\n';
    std::cerr << "------------------------\n";

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
            for (auto &b : letin.bindings())
                bindings_->push_back(b);
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
    GlobalizeFunctions gf;
    ast->accept(gf);

    //PeelLetIns pi;
    //ast->accept(pi);
}

} } } }
