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

    REQUIRE(resolve_type(to_ast("1 + 2.0")) == "<int+float>");
    REQUIRE(resolve_type(to_ast("1.0 + 2 + 3.0")) == "<<float+int>+float>");
    REQUIRE(resolve_type(to_ast("(1.0 + 2.0) / 3")) == "<float/int>");
    REQUIRE(resolve_type(to_ast("(1.0 + 2) * 3.0")) == "<<float+int>*float>");

    REQUIRE(resolve_type(to_ast("let x = 1 in 1")) == "int");
    REQUIRE(resolve_type(to_ast("let x = 1.0 in 1.0")) == "float");
    REQUIRE(resolve_type(to_ast("let x = 1+1 in 1+1")) == "int");
    REQUIRE(resolve_type(to_ast("let x = 1+1.0 in 1+1.0")) == "<int+float>");

    // resolve_type does not do symbol lookup, therefore:
    REQUIRE(resolve_type(to_ast("let x = 1 in x")) == "<id>");
    REQUIRE(resolve_type(to_ast("let x = 1.0 in x")) == "<id>");
    REQUIRE(resolve_type(to_ast("let x = 1+1 in x")) == "<id>");
    REQUIRE(resolve_type(to_ast("let x = 1+1.0 in x+y*z")) == "<<id>+<<id>*<id>>>");
    REQUIRE(resolve_type(to_ast("let x = 1+1.0 in x/y-z")) == "<<<id>/<id>>-<id>>");
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

        stack<string> scope;

        void reduce_binary(string op) {
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (1)");
            const string rhs = scope.top();
            scope.pop();
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (2)");
            const string lhs = scope.top();
            scope.pop();

            // <id> as a special case. Symbol Lookup is not resolve_type()-responsibility.
            if ((lhs != "<id>") && (lhs == rhs)) {
                scope.push(lhs);
            } else {
                scope.push("<" + lhs + op + rhs + ">"); // << possibly use this derived name for operator overloading in the future
            }
        }

    public:
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

        void begin(Call const &) { scope.push("<call>"); }
        void end(Call const &) {}

        void begin(Negation const &) {}
        void end(Negation const &) {}

        void begin(ParenExpression const &) {}
        void end(ParenExpression const &) {}

        void begin(Binding const &) {}
        void end(Binding const &) {}

        void begin(AST::Identifier const &) { scope.push("<id>"); }
        void end(AST::Identifier const &) {}

        void begin(LetIn const &) {}
        void before_body(LetIn const &) {}
        void end(LetIn const &) {
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (1)");
            const string rhs = scope.top();
            scope.pop();
            if (scope.empty()) throw std::logic_error("reduce_binary: empty stack (2)");
            const string lhs = scope.top();
            scope.pop();
            scope.push(rhs);
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

    };

    std::string try_resolve (ASTNode const &ast) {
        TryResolve tr;
        ast.accept (tr);
        return tr.type();
    }

    struct ResolveTypes final : Transform {
        bool transformed() const { return transformed_; }
        bool has_unresolved() const { return has_unresolved_; }

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
            std::cout << "???" << binding.id() << " <-- " << try_resolve(binding.body()) << std::endl;
        }
        void end(Binding &)
        {
        }

        void begin(AST::Identifier &) {}
        void end(AST::Identifier &) {}

        void begin(LetIn &) {}
        void end(LetIn &) {}

        void begin(Program &) {}
        void end(Program &) {}

    private:
         bool transformed_ = false;
         bool has_unresolved_ = false;
    };
}

std::string resolve_type(shared_ptr<AST::ASTNode> ast)
{
    if (!ast) {
        //std::clog << "pass: lambda-lifting skipped, AST is empty\n";
        return "<void>";
    }
    return resolve_type(*ast);
}

std::string resolve_type(AST::ASTNode const &ast)
{
    TryResolve tr;
    ast.accept(tr);
    return tr.type();
}


} } } }
