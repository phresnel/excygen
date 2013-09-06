// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PRETTYPRINTER_HH_INCLUDED_20130821
#define PRETTYPRINTER_HH_INCLUDED_20130821

#include "../AST.hh"
#include <iostream>
#include <stack>

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPrinters {

    struct PrettyPrinter final : AST::Visitor {

        PrettyPrinter() {
            scope.push({""});
        }

        PrettyPrinter(std::ostream &os) : os(os) {
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
        void visit(AST::BoolLiteral const &lit) { os << lit.value(); }

        void visit(AST::Identifier const &id) { os << id.id(); }

        void begin(AST::Call const &call)
        {
            scope.push({", "});
            os << call.id() << "(";
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
            //if (scope.top().argCount++) os << scope.top().Operator;
            scope.push({""});
            indent();
            if (binding.type())
                os << binding.type().name() << ' ' << binding.id();
            else
                os << binding.id();


            if (!binding.arguments().empty()) {
                os << "(";
                bool first = true;
                for (auto a : binding.arguments()) {
                    if (!first) {
                        os << ", ";
                    }
                    first = false;
                    if (a.type) {
                        os << a.type.name() << ' ' << a.name;
                    } else {
                        os << a.name; // I consider omitting "auto" good practice for readability.
                                      // (e.g. 'f(auto x, auto y)' vs. 'f(x,y)')
                    }
                }
                os << ")";
            }
            os << " = ";
        }
        void end(AST::Binding const &)
        {
            scope.pop();
        }

        void begin(AST::IfThenElse const &ite)
        {
            os << "if ";
        }
        void before_then()
        {
            os << "\n";
            indent();
            os << "then ";
        }
        void before_else()
        {
            os << "\n";
            indent();
            os << "else ";
        }
        void end(AST::IfThenElse const &ite) {}

        void begin(AST::LetIn const &letin)
        {
            os << "let\n";
            ++indent_; ++indent_;
            scope.push({",\n"});
        }
        void before_body(AST::LetIn const &)
        {
            --indent_;
            os << '\n';
            indent(-1); os << "in ";
            scope.pop();
        }
        void end(AST::LetIn const &)
        {
            --indent_;
        }

        void begin(AST::Program const &prog)
        {
            if (prog.bindings().empty())
                return;
            os << "program\n";
            ++indent_; ++indent_;
            scope.push({",\n"});
        }
        void before_body(AST::Program const &prog)
        {
            if (prog.bindings().empty())
                return;
            --indent_;
            os << '\n';
            indent(-1); os << "in ";
            scope.pop();
        }
        void end(AST::Program const &prog)
        {
            if (prog.bindings().empty())
                return;
            --indent_;
        }

    private:
        std::ostream &os = std::cout;
        int indent_ = 0;
        void indent(int shift=0) {
            for (int i=0; i!=(indent_*2)+shift; ++i) {
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

    std::string pretty_print(std::string in);
    std::string pretty_print(AST::ASTNode const &ast);

} } } }

#endif // PRETTYPRINTER_HH_INCLUDED_20130821
