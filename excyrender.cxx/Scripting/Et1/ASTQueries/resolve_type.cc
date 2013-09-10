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
#include "../Backends/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTQueries/resolve_type.hh", "Type resolution" ) {
    using namespace excyrender::Nature::Et1;
    using detail::to_ast;
    using namespace ASTQueries;
    using AST::Typeinfo;

    // Tests without symbol table.
    REQUIRE(resolve_type(to_ast("true")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("false")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("true && true")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("false && false")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("true || true")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("false || false")) == Typeinfo("bool"));

    REQUIRE(resolve_type_raw(to_ast("x && true")) == ("<<id>&&bool>"));
    REQUIRE(resolve_type_raw(to_ast("x && false")) == ("<<id>&&bool>"));
    REQUIRE(resolve_type_raw(to_ast("x || true")) == ("<<id>||bool>"));
    REQUIRE(resolve_type_raw(to_ast("x || false")) == ("<<id>||bool>"));
    REQUIRE(resolve_type_raw(to_ast("x < true")) == ("<<id><bool>"));
    REQUIRE(resolve_type_raw(to_ast("x > false")) == ("<<id>>bool>"));
    REQUIRE(resolve_type_raw(to_ast("x <= true")) == ("<<id><=bool>"));
    REQUIRE(resolve_type_raw(to_ast("x >= false")) == ("<<id>>=bool>"));

    REQUIRE(resolve_type(to_ast("!true")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("!!!!!true")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("-1")) == Typeinfo("int"));
    REQUIRE(resolve_type(to_ast("-----1")) == Typeinfo("int"));
    REQUIRE(resolve_type(to_ast("(-1.0)")) == Typeinfo("float"));
    REQUIRE(resolve_type(to_ast("--(---1.0)")) == Typeinfo("float"));
    REQUIRE(resolve_type_raw(to_ast("(!!(false) && (1<-2))")) == "bool");

    REQUIRE(resolve_type(to_ast("1<2")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("1.0>2.0")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("true==!false")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("!false!=!true")) == Typeinfo("bool"));

    REQUIRE(resolve_type(to_ast("if 1<2 then true else false")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("if 1<2 then 1 else 2")) == Typeinfo("int"));
    REQUIRE(resolve_type(to_ast("if 1<2 then if 1<2 then if 1<2 then 1 else 2 else 2 else 2")) == Typeinfo("int"));

    REQUIRE(resolve_type_raw(to_ast("if true then 1 else false")) == ("<bool?int:bool>"));
    REQUIRE(resolve_type(to_ast("if true then true else false")) == Typeinfo("bool"));
    REQUIRE(resolve_type_raw(to_ast("if A then B else C")) == ("<<id>?<id>:<id>>"));
    REQUIRE(resolve_type_raw(to_ast("if true then B else C")) == ("<bool?<id>:<id>>"));
    REQUIRE(resolve_type_raw(to_ast("if A then B else false")) == ("<<id>?<id>:bool>"));
    REQUIRE(resolve_type_raw(to_ast("if A then if B then C else D else E")) == ("<<id>?<<id>?<id>:<id>>:<id>>"));
    REQUIRE(resolve_type_raw(to_ast("if A then E else if B then C else D")) == ("<<id>?<id>:<<id>?<id>:<id>>>"));
    REQUIRE(resolve_type_raw(to_ast("if A then if B then C else D else if E then F else G ")) == ("<<id>?<<id>?<id>:<id>>:<<id>?<id>:<id>>>"));
    REQUIRE(resolve_type(to_ast("if true||false then false&&true else if false then true else false")) == Typeinfo("bool"));

    REQUIRE(resolve_type(to_ast("1")) == Typeinfo("int"));
    REQUIRE(resolve_type(to_ast("1 + 2")) == Typeinfo("int"));
    REQUIRE(resolve_type(to_ast("1 + 2 + 3")) == Typeinfo("int"));
    REQUIRE(resolve_type(to_ast("(1 + 2) + 3")) == Typeinfo("int"));
    REQUIRE(resolve_type(to_ast("(1 + 2) * 3")) == Typeinfo("int"));

    REQUIRE(resolve_type(to_ast("1 > 2")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("1 <= 2 && 2 >= 3")) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("(1 == 2) != (2!=0)")) == Typeinfo("bool"));

    REQUIRE(resolve_type_raw(to_ast("x > 2")) == ("<<id>>int>"));
    REQUIRE(resolve_type_raw(to_ast("1 <= y && zulu >= 3")) == ("<<int<=<id>>&&<<id>>=int>>"));
    REQUIRE(resolve_type_raw(to_ast("1 <= 2 && zulu >= 3")) == ("<bool&&<<id>>=int>>"));
    REQUIRE(resolve_type_raw(to_ast("(1 == 2) != (2!=0)")) == ("bool"));

    REQUIRE(resolve_type(to_ast("1.0")) == Typeinfo("float"));
    REQUIRE(resolve_type(to_ast("1.0 + 2.0")) == Typeinfo("float"));
    REQUIRE(resolve_type(to_ast("1.0 + 2.0 + 3.0")) == Typeinfo("float"));
    REQUIRE(resolve_type(to_ast("(1.0 + 2.0) + 3.0")) == Typeinfo("float"));
    REQUIRE(resolve_type(to_ast("(1.0 + 2.0) * 3.0")) == Typeinfo("float"));

    REQUIRE(resolve_type_raw(to_ast("(1.0 + (2.0-x)) * 3.0")) == "<<float+<float-<id>>>*float>");

    REQUIRE(resolve_type_raw(to_ast("(2*x)+(2*x)")) == "<<int*<id>>+<int*<id>>>");

    REQUIRE(resolve_type_raw(to_ast("1 + 2.0")) == "<int+float>");
    REQUIRE(resolve_type_raw(to_ast("1.0 + 2 + 3.0")) == "<<float+int>+float>");
    REQUIRE(resolve_type_raw(to_ast("(1.0 + 2.0) / 3")) == "<float/int>");
    REQUIRE(resolve_type_raw(to_ast("(1.0 + 2) * 3.0")) == "<<float+int>*float>");

    REQUIRE(resolve_type(to_ast("let x = 1 in 1")) == Typeinfo("int"));
    REQUIRE(resolve_type(to_ast("let x = 1.0 in 1.0")) == Typeinfo("float"));
    REQUIRE(resolve_type(to_ast("let x = 1+1 in 1+1")) == Typeinfo("int"));
    REQUIRE(resolve_type_raw(to_ast("let x = 1+1.0 in 1+1.0")) == "<int+float>");

    REQUIRE(resolve_type_raw(to_ast("let x = 1 in x")) == "<id>");
    REQUIRE(resolve_type_raw(to_ast("let x = 1.0 in x")) == "<id>");
    REQUIRE(resolve_type_raw(to_ast("let x = 1+1 in x")) == "<id>");
    REQUIRE(resolve_type_raw(to_ast("let x = 1+1.0 in x+y*z")) == "<<id>+<<id>*<id>>>");
    REQUIRE(resolve_type_raw(to_ast("let x = 1+1.0 in x/y-z")) == "<<<id>/<id>>-<id>>");

    REQUIRE(resolve_type_raw(to_ast("f()")) == "<call>");
    REQUIRE(resolve_type_raw(to_ast("f(x)")) == "<call>");
    REQUIRE(resolve_type_raw(to_ast("f(x)+1")) == "<<call>+int>");
    REQUIRE(resolve_type_raw(to_ast("let int f(x) = 0.0 in f(1)")) == "<call>");

    REQUIRE(resolve_type_raw(to_ast("let f(x) = x in f(2)")) == "<call>");

    // Tests with symbol table.
    REQUIRE(resolve_type(to_ast("x"), {{"x",Typeinfo("int")}}) == Typeinfo("int"));
    REQUIRE(resolve_type_raw(to_ast("x+y"), {{"x",Typeinfo("int")}}) == "<int+<id>>");
    REQUIRE(resolve_type(to_ast("x+y"),
                         {{"x",Typeinfo("int")},
                          {"y",Typeinfo("int")}})
                  == Typeinfo("int"));
    REQUIRE(resolve_type_raw(to_ast("x/y"),
                             {{"x",Typeinfo("float")},
                              {"y",Typeinfo("int")}})
                  == "<float/int>");

    REQUIRE(resolve_type(to_ast("x"), {{"x",Typeinfo("bool")}}) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("x && x"), {{"x",Typeinfo("bool")}}) == Typeinfo("bool"));
    REQUIRE(resolve_type(to_ast("x && true"), {{"x",Typeinfo("bool")}}) == Typeinfo("bool"));
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
        TryResolve(std::map<string,Typeinfo> const & symbols) : symbols(symbols) {}

        void begin(AST::Addition const &) {}
        void end(AST::Addition const &) { reduce_binary("+"); }
        void begin(AST::Subtraction const &) {}
        void end(AST::Subtraction const &) { reduce_binary("-"); }
        void begin(AST::Multiplication const &) {}
        void end(AST::Multiplication const &) { reduce_binary("*"); }
        void begin(AST::Division const &) {}
        void end(AST::Division const &) { reduce_binary("/"); }

        void begin(AST::LessThan const &)  {}
        void end(AST::LessThan const &)  { reduce_binary("<", "bool"); }
        void begin(AST::LessEqual const &)  {}
        void end(AST::LessEqual const &)  { reduce_binary("<=", "bool"); }
        void begin(AST::GreaterThan const &)  {}
        void end(AST::GreaterThan const &)  { reduce_binary(">", "bool"); }
        void begin(AST::GreaterEqual const &)  {}
        void end(AST::GreaterEqual const &)  { reduce_binary(">=", "bool"); }
        void begin(AST::Equal const &)  {}
        void end(AST::Equal const &)  { reduce_binary("==", "bool"); }
        void begin(AST::NotEqual const &)  {}
        void end(AST::NotEqual const &)  { reduce_binary("!=", "bool"); }
        void begin(AST::LogicalAnd const &) {}
        void end(AST::LogicalAnd const &) { reduce_binary("&&", "bool"); }
        void begin(AST::LogicalOr const &) {}
        void end(AST::LogicalOr const &) { reduce_binary("||", "bool"); }
        void begin(AST::LogicalNot const &) {}
        void end(AST::LogicalNot const &) {}

        void begin(AST::IfThenElse const &) {}
        void end(AST::IfThenElse const &) { reduce_ifthenelse(); }

        void infix() {}

        void visit(IntegerLiteral const &) { scope.push("int"); }
        void visit(RealLiteral const &) { scope.push("float"); }
        void visit(BoolLiteral const &) { scope.push("bool"); }

        void visit(AST::Identifier const &id)
        {
            // if already resolved, use it
            if (id.type()) {
                scope.push(id.type().name());
                return;
            }

            // else, try resolve
            auto e = symbols.find(id.id());
            if (e!=symbols.end() && e->second) {
                scope.push(e->second.name());
            } else {
                scope.push("<id>");
            }
        }

        void begin(AST::Call const &call) {
            if (!call.type()) {
                scope.push("<call>");
            } else {
                scope.push(call.type().name());
            }
        }
        void end(AST::Call const &call) {
            // pop the arguments away.
            for (size_t i=0; i!=call.arguments().size(); ++i) {
                if (scope.empty())
                    throw std::logic_error("empty stack upon popping arguments");
                scope.pop();
            }
        }

        void begin(AST::Negation const &) {}
        void end(AST::Negation const &) {}

        void begin(AST::ParenExpression const &) {}
        void end(AST::ParenExpression const &) {}

        void begin(AST::Binding const &) {}
        void end(AST::Binding const &) {}

        void begin(AST::LetIn const &) {}
        void before_body(LetIn const &letin) {
            for (size_t i=0; i!=letin.bindings().size(); ++i) {
                if (scope.empty())
                    throw std::logic_error("empty stack upon popping bindings");
                scope.pop();
            }
        }
        void end(AST::LetIn const &) {
            /*if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (1)");
            const string rhs = scope.top();
            scope.pop();
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (2)");
            scope.pop();
            scope.push(rhs);*/
        }

        void begin(AST::Program const &) {}
        void end(AST::Program const &) {}

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
        void reduce_binary(string op, string result_type = "") {
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
                scope.push(result_type != "" ? result_type : rhs);
            } else {
                scope.push("<" + lhs + op + rhs + ">"); // << possibly use this derived name for operator overloading in the future
            }
        }

        void reduce_ifthenelse() {
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (1)");
            const string else_ = scope.top();
            scope.pop();

            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (2)");
            const string then_ = scope.top();
            scope.pop();

            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (3)");
            const string if_ = scope.top();
            scope.pop();

            if (if_.empty() || then_.empty() || else_.empty()) {
                throw std::logic_error("empty type pushed (2)");
            }

            // Forbid reduction of non-reduced types by checking against '<'.
            if ((if_[0] != '<') && (then_[0] != '<') && (else_[0] != '<') && (then_ == else_)) {
                scope.push(then_);
            } else {
                scope.push("<" + if_ + "?" + then_ + ":" + else_ + ">"); // << possibly use this derived name for operator overloading in the future
            }
        }

    private:
        stack<string> scope;
        std::map<string,Typeinfo> symbols;

    };
}



string resolve_type_raw(shared_ptr<ASTNode> ast)
{
    if (!ast) return "<void>";
    return resolve_type_raw(*ast);
}

string resolve_type_raw(ASTNode const &ast)
{
    std::map<string,Typeinfo> none;
    return resolve_type_raw(ast, none);
}

string resolve_type_raw(shared_ptr<ASTNode> ast, std::map<string,Typeinfo> const &symbols)
{
    if (!ast) return "<void>";
    return resolve_type_raw(*ast, symbols);
}

string resolve_type_raw(ASTNode const &ast, std::map<string,Typeinfo> const &symbols)
{
    TryResolve tr(symbols);
    ast.accept(tr);
    return tr.type();
}


Typeinfo resolve_type(shared_ptr<ASTNode> ast)
{
    if (!ast) return Typeinfo();
    return resolve_type(*ast);
}

Typeinfo resolve_type(ASTNode const &ast)
{
    std::map<string,Typeinfo> const symbols;
    return resolve_type(ast, symbols);
}

Typeinfo resolve_type(shared_ptr<ASTNode> ast, std::map<string,Typeinfo> const &symbols)
{
    if (!ast) return Typeinfo();
    return resolve_type(*ast, symbols);
}

Typeinfo resolve_type(ASTNode const &ast, std::map<string,Typeinfo> const &symbols)
{
    if (ast.type())
        return ast.type();
    auto t = resolve_type_raw(ast, symbols);
    if (!t.empty() && t[0] != '<') return Typeinfo(t);
    return Typeinfo();
}

} } } }
