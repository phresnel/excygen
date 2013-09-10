// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Python.hh"
#include "Python.h"
#include "../UnitTesting.hh"
#include <stack>

#include "../ASTPasses/1200_globalize_functions.hh"
#include "../ASTPasses/1150_mangle.hh"
#include "../ASTPasses/1100_resolve_types.hh"
#include "../ASTPasses/1000_lambda_lift.hh"
#include "../Backends/PrettyPrinter.hh"


//--------------------------------------------------------------------------------------------------
TEST_CASE( "Et1/Backends/Python", "Python backend" ) {

    using namespace excyrender::Nature::Et1;
    using namespace excyrender::Nature::Et1::Backends;
    using namespace excyrender::Nature::Et1::ASTPrinters;


    //std::string py = "let f(x) = x*2.0, z=let foo(frob)=frob+1.0 in if foo(1.0) < f(3.0) then 1 else 2 in z";

    std::string et = "program (float u, float v) = let x=2.1, y=7.5, f(x)=y in if x<(y/4.0) then f(1) else 1.0";
    std::string c = to_python(et);

    auto prepped = detail::to_ast(et);
    ASTPasses::lambda_lift(prepped);
    ASTPasses::resolve_types(prepped);
    ASTPasses::mangle(prepped);
    ASTPasses::globalize_functions(prepped);

    std::cerr << "--------------------\n";
    std::cerr << pretty_print(et) << std::endl;
    std::cerr << "--------------------\n";
    std::cerr << pretty_print(*prepped) << std::endl;
    std::cerr << "--------------------\n";
    std::cerr << c << std::endl;
    std::cerr << "--------------------\n";


}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Nature { namespace Et1 { namespace Backends { namespace {

    std::string pythonize_id(std::string id) {
        for(auto &c : id)
            c = c=='$' ? '_' : c;
        return id;
    }

    struct PythonPrinter final : AST::Visitor {

        PythonPrinter() {
            scope.push({""});
        }

        PythonPrinter(std::ostream &os) : os(os) {
            scope.push({""});
        }

        void begin(AST::Addition const &) { scope.push({" + "}); }
        void end(AST::Addition const &) { scope.pop(); }
        void begin(AST::Subtraction const &) { scope.push({" - "}); }
        void end(AST::Subtraction const &) { scope.pop(); }
        void begin(AST::Multiplication const &) { scope.push({" * "}); }
        void end(AST::Multiplication const &) { scope.pop(); }
        void begin(AST::Division const &) { scope.push({" / "}); }
        void end(AST::Division const &) { scope.pop(); }

        void begin(AST::LessThan const &)  { scope.push({" < "}); }
        void end(AST::LessThan const &)  { scope.pop(); }
        void begin(AST::LessEqual const &)  { scope.push({" <= "}); }
        void end(AST::LessEqual const &)  { scope.pop(); }
        void begin(AST::GreaterThan const &)  { scope.push({" > "}); }
        void end(AST::GreaterThan const &)  { scope.pop(); }
        void begin(AST::GreaterEqual const &)  { scope.push({" >= "}); }
        void end(AST::GreaterEqual const &)  { scope.pop(); }
        void begin(AST::Equal const &)  { scope.push({" == "}); }
        void end(AST::Equal const &)  { scope.pop(); }
        void begin(AST::NotEqual const &)  { scope.push({" != "}); }
        void end(AST::NotEqual const &)  { scope.pop(); }
        void begin(AST::LogicalAnd const &) { scope.push({" && "}); }
        void end(AST::LogicalAnd const &) { scope.pop(); }
        void begin(AST::LogicalOr const &) { scope.push({" || "}); }
        void end(AST::LogicalOr const &) { scope.pop(); }
        void begin(AST::LogicalNot const &) { os << " !"; }
        void end(AST::LogicalNot const &) {}

        void infix()
        {
            if (!scope.empty()) os << scope.top().Operator;
        }

        void begin(AST::Negation const &) { os << " -"; }
        void end(AST::Negation const &) { }

        void visit(AST::IntegerLiteral const &lit) { os << lit.value(); }
        void visit(AST::RealLiteral const &lit) { os << lit.value(); }
        void visit(AST::BoolLiteral const &lit) { os << (lit.value() == "true" ? "True" : "False"); }

        void visit(AST::Identifier const &id) { os << pythonize_id(id.id()); }

        void begin(AST::Call const &call)
        {
            scope.push({", "});
            os << pythonize_id(call.id()) << "(";
        }
        void end(AST::Call const &)
        {
            os << ")";
            scope.pop();
        }

        void begin(AST::ParenExpression const &call) { os << "("; }
        void end(AST::ParenExpression const &) { os << ")"; }

        void begin(AST::Binding const &binding)
        {
            scope.push({""});
            if (!binding.arguments().empty()) {
                os << '\n';
                indent();
                os << "def " << pythonize_id(binding.id()) << "(";

                bool first = true;
                for (auto a : binding.arguments()) {
                    if (!first) os << ", ";
                    first = false;
                    os << pythonize_id(a.name);
                }
                os << "): ";
            } else {
                os << '\n';
                indent();
                os << pythonize_id(binding.id()) << " = ";
            }
        }
        void end(AST::Binding const &)
        {
            scope.pop();
        }

        void begin(AST::IfThenElse const &ite)
        {
            os << "__ternary(";
        }
        void before_then()
        {
            os << ", lambda: ";
        }
        void before_else()
        {
            os << ", lambda: ";
        }
        void end(AST::IfThenElse const &ite) {
            os << ")";
        }

        void begin(AST::LetIn const &letin)
        {
            //os << '\n';
            scope.push({""});
            ++indent_;
        }
        void before_body(AST::LetIn const &)
        {
            os << '\n';
            indent();
            os << "return ";
        }
        void end(AST::LetIn const &)
        {
            scope.pop();
            --indent_;
        }

        void begin(AST::Program const &id)
        {
            //scope.push({"\n"});
            os << "def __ternary(cond,t,e): \n    return t() if cond else e()\n\n";
            /*os << "def __et1_fun_main():";
            ++indent_;*/
        }
        void before_body(AST::Program const &)
        {
        }
        void end(AST::Program const &)
        {
            //scope.pop();
            //--indent_;
        }

    private:
        std::ostream &os = std::cout;
        int indent_ = 0;
        void indent(int shift=0) {
            for (int i=0; i!=(indent_*4)+shift; ++i) {
                os << ' ';
            }
        }

        struct Operation {
            string Operator;
            int argCount = 0;

            Operation(string Operator) : Operator(Operator) {}
        };
        std::stack<Operation> scope;
    };


} } } } }


namespace excyrender { namespace Nature { namespace Et1 { namespace Backends {

string to_python(string et1)
{
    using namespace excyrender::Nature::Et1::detail;

    std::stringstream ss;
    PythonPrinter pp(ss);
    auto ast = to_ast(et1);

    ASTPasses::lambda_lift(ast);
    ASTPasses::resolve_types(ast);
    ASTPasses::mangle(ast);
    ASTPasses::globalize_functions(ast);
    ast->accept(pp);
    return ss.str();
}

} } } }
