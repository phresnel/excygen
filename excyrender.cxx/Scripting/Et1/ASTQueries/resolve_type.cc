// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "resolve_type.hh"

#include <stack>
#include <iostream>
#include <stdexcept>
#include <algorithm>



//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../ASTPrinters/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTQueries/resolve_type.hh", "Type resolution" ) {
    using namespace excyrender::Nature::Et1;
    using detail::to_ast;
    using namespace ASTQueries;

    // Tests without symbol table.
    REQUIRE(resolve_type(to_ast("1")) == "int");
    REQUIRE(resolve_type(to_ast("1 + 2")) == "int");
    REQUIRE(resolve_type(to_ast("1 + 2 + 3")) == "int");
    REQUIRE(resolve_type(to_ast("(1 + 2) + 3")) == "int");
    REQUIRE(resolve_type(to_ast("(1 + 2) * 3")) == "int");

    REQUIRE(resolve_type(to_ast("1.0")) == "float");
    REQUIRE(resolve_type(to_ast("1.0 + 2.0")) == "float");
    REQUIRE(resolve_type(to_ast("1.0 + 2.0 + 3.0")) == "float");
    REQUIRE(resolve_type(to_ast("(1.0 + 2.0) + 3.0")) == "float");
    REQUIRE(resolve_type(to_ast("(1.0 + 2.0) * 3.0")) == "float");

    REQUIRE(resolve_type(to_ast("(1.0 + (2.0-x)) * 3.0")) == "<<float+<float-<id>>>*float>");

    REQUIRE(resolve_type(to_ast("(2*x)+(2*x)")) == "<<int*<id>>+<int*<id>>>");

    REQUIRE(resolve_type(to_ast("1 + 2.0")) == "<int+float>");
    REQUIRE(resolve_type(to_ast("1.0 + 2 + 3.0")) == "<<float+int>+float>");
    REQUIRE(resolve_type(to_ast("(1.0 + 2.0) / 3")) == "<float/int>");
    REQUIRE(resolve_type(to_ast("(1.0 + 2) * 3.0")) == "<<float+int>*float>");

    REQUIRE(resolve_type(to_ast("let x = 1 in 1")) == "int");
    REQUIRE(resolve_type(to_ast("let x = 1.0 in 1.0")) == "float");
    REQUIRE(resolve_type(to_ast("let x = 1+1 in 1+1")) == "int");
    REQUIRE(resolve_type(to_ast("let x = 1+1.0 in 1+1.0")) == "<int+float>");

    REQUIRE(resolve_type(to_ast("let x = 1 in x")) == "<id>");
    REQUIRE(resolve_type(to_ast("let x = 1.0 in x")) == "<id>");
    REQUIRE(resolve_type(to_ast("let x = 1+1 in x")) == "<id>");
    REQUIRE(resolve_type(to_ast("let x = 1+1.0 in x+y*z")) == "<<id>+<<id>*<id>>>");
    REQUIRE(resolve_type(to_ast("let x = 1+1.0 in x/y-z")) == "<<<id>/<id>>-<id>>");

    REQUIRE(resolve_type(to_ast("f()")) == "<call>");
    REQUIRE(resolve_type(to_ast("f(x)")) == "<call>");
    REQUIRE(resolve_type(to_ast("f(x)+1")) == "<<call>+int>");
    REQUIRE(resolve_type(to_ast("let int f(x) = 0.0 in f(1)")) == "<call>");

    REQUIRE(resolve_type(to_ast("let f(x) = x in f(2)")) == "<call>");

    // Tests with symbol table.
    REQUIRE(resolve_type(to_ast("x"), {{"x","int"}}) == "int");
    REQUIRE(resolve_type(to_ast("x+y"), {{"x","int"}}) == "<int+<id>>");
    REQUIRE(resolve_type(to_ast("x+y"), {{"x","int"}, {"y","int"}}) == "int");
    REQUIRE(resolve_type(to_ast("x/y"), {{"x","float"}, {"y","int"}}) == "<float/int>");
}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Nature { namespace Et1 { namespace ASTQueries {

namespace {
    using namespace AST;
    using std::set;
    using std::stack;
    using std::string;

    // TODO: extract TryResolve to its own unit and test it extensively.
    class TryResolve final : public Visitor {
    public:
        TryResolve() = default;
        TryResolve(std::map<string,string> const & symbols) : symbols(symbols) {}

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

        void begin(RealLiteral const &) { scope.push("float"); }
        void end(RealLiteral const &) {}

        void begin(Call const &call) {
            if (call.type() == "auto") {
                scope.push("<call>");
            } else {
                scope.push(call.type());
            }
        }
        void end(Call const &call) {
            // pop the arguments away.
            for (size_t i=0; i!=call.arguments().size(); ++i) {
                if (scope.empty())
                    throw std::logic_error("empty stack upon popping arguments");
                scope.pop();
            }
        }

        void begin(Negation const &) {}
        void end(Negation const &) {}

        void begin(ParenExpression const &) {}
        void end(ParenExpression const &) {}

        void begin(Binding const &) {}
        void end(Binding const &) {}

        void begin(AST::Identifier const &id)
        {
            auto e = symbols.find(id.id());
            if (e!=symbols.end()) {
                scope.push(e->second);
            } else {
                scope.push("<id>");
            }
        }
        void end(AST::Identifier const &) {}

        void begin(LetIn const &) {}
        void before_body(LetIn const &letin) {
            for (size_t i=0; i!=letin.bindings().size(); ++i) {
                if (scope.empty())
                    throw std::logic_error("empty stack upon popping bindings");
                scope.pop();
            }
        }
        void end(LetIn const &) {
            /*if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (1)");
            const string rhs = scope.top();
            scope.pop();
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (2)");
            scope.pop();
            scope.push(rhs);*/
        }

        void begin(Program const &) {}
        void end(Program const &) {}

        std::string type() const {
            if (scope.empty()) {
                throw std::logic_error("TryResolve type stack empty when type() is called");
            }
            if (scope.size()>1) {
                string content;
                auto sc = scope;
                while (!sc.empty()) {
                    content += "[" + sc.top() + "]";
                    sc.pop();
                }
                throw std::logic_error("TryResolve type stack not fully reduced when type() is called "
                                       "{" + content + "}");
            }
            return scope.top();
        }


    private:
        void reduce_binary(string op) {
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (1)");
            const string rhs = scope.top();
            scope.pop();
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (2)");
            const string lhs = scope.top();
            scope.pop();

            if (lhs.empty() || rhs.empty()) {
                throw std::logic_error("empty type pushed");
            }

            // Forbid reduction of non-reduced types by checking against '<'.
            if ((lhs[0] != '<') && (rhs[0] != '<') && (lhs == rhs)) {
                scope.push(lhs);
            } else {
                scope.push("<" + lhs + op + rhs + ">"); // << possibly use this derived name for operator overloading in the future
            }
        }

    private:
        stack<string> scope;
        std::map<string,string> symbols;

    };
}



std::string resolve_type(shared_ptr<AST::ASTNode> ast)
{
    std::map<string,string> none;
    return resolve_type(ast, none);
}

std::string resolve_type(AST::ASTNode const &ast)
{
    std::map<string,string> none;
    return resolve_type(ast, none);
}

std::string resolve_type(shared_ptr<AST::ASTNode> ast, std::map<string,string> const &symbols)
{
    if (!ast) {
        //std::clog << "pass: lambda-lifting skipped, AST is empty\n";
        return "<void>";
    }
    return resolve_type(*ast, symbols);
}

std::string resolve_type(AST::ASTNode const &ast, std::map<string,string> const &symbols)
{
    TryResolve tr(symbols);
    ast.accept(tr);
    return tr.type();
}

} } } }
