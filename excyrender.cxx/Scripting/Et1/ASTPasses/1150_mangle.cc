// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "1150_mangle.hh"
#include "1100_resolve_types.hh"
#include "1000_lambda_lift.hh"


//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../Backends/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTPasses/1150_mangle", "Name mangling" ) {
    using std::string;
    using namespace excyrender::Nature::Et1;
    using namespace excyrender::Nature::Et1::ASTPrinters;
    using namespace excyrender::Nature::Et1::ASTPasses;


    string in = "let f(x) = let g(x) = x in g(x) in f(1)";
    auto ast = detail::to_ast(in);

    std::cerr << "------------------------\n";
    std::cerr << pretty_print(*ast) << '\n';
    std::cerr << "------------------------\n";
    lambda_lift(ast);
    resolve_types(ast);
    mangle(ast);
    std::cerr << pretty_print(*ast) << '\n';
    std::cerr << "------------------------\n";

}
//- Tests ------------------------------------------------------------------------------------------


namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses { namespace {

    struct Mangle : AST::Transform {
        Mangle() { N.push("$"); }

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

        void begin(AST::Binding &b) {
            string id = b.id();
            string arg_part;
            for (auto &a : b.arguments())
                arg_part += a.type.name() + "$";
            b.reset_id(N.top() + id + "$_$" + arg_part);
            N.push(N.top() + id + "$");
        }
        void end(AST::Binding &) {
            N.pop();
        }

        void begin(AST::IfThenElse &) {}
        void end(AST::IfThenElse &) {}

        void begin(AST::LetIn &) {}
        void end(AST::LetIn &) {}

        void begin(AST::Program &) {}
        void end(AST::Program &) {}

    private:
        std::stack<std::string> N;
    };
} } } } }


namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

void mangle(shared_ptr<AST::ASTNode> ast)
{
    Mangle m;
    ast->accept(m);
}

} } } }
