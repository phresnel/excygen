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

        void begin(AST::Addition const &)
        {
            scope.push({" + "});
        }
        void end(AST::Addition const &)
        {
            scope.pop();
        }

        void begin(AST::Subtraction const &)
        {
            scope.push({" - "});
        }
        void end(AST::Subtraction const &)
        {
            scope.pop();
        }

        void begin(AST::Negation const &)
        {
            os << "-";
        }
        void end(AST::Negation const &)
        {
        }

        void begin(AST::Multiplication const &)
        {
            scope.push({" * "});
        }
        void end(AST::Multiplication const &)
        {
            scope.pop();
        }

        void begin(AST::Division const &)
        {
            scope.push({" / "});
        }
        void end(AST::Division const &)
        {
            scope.pop();
        }

        void begin(AST::IntegerLiteral const &lit)
        {
            if (scope.top().argCount++) os << scope.top().Operator;
            os << lit.value();
        }
        void end(AST::IntegerLiteral const &)
        {
        }

        void begin(AST::Call const &call)
        {
            if (scope.top().argCount++) os << scope.top().Operator;
            scope.push({", "});
            os << call.id() << "(";
        }
        void end(AST::Call const &)
        {
            os << ")";
            scope.pop();
        }

        void begin(AST::ParenExpression const &call)
        {
            if (scope.top().argCount++) os << scope.top().Operator;
            os << "(";
        }
        void end(AST::ParenExpression const &)
        {
            os << ")";
        }

        void begin(AST::Binding const &binding)
        {
            indent(); os << binding.id();
            if (!binding.arguments().empty()) {
                os << "(";
                bool first = true;
                for (auto a : binding.arguments()) {
                    if (!first) {
                        os << ", ";
                    }
                    first = false;
                    os << a.name;
                }
                os << ")";
            }
            os << " = ";
        }
        void end(AST::Binding const &)
        {
            os << '\n';
        }

        void begin(AST::Identifier const &id)
        {
            if (scope.top().argCount++) os << scope.top().Operator;
            os << id.id();
        }
        void end(AST::Identifier const &)
        {
        }

        void begin(AST::LetIn const &letin)
        {
            os << "let\n";
            ++indent_; ++indent_;
        }
        void before_body(AST::LetIn const &)
        {
            --indent_;
            indent(-1); os << "in ";
        }
        void end(AST::LetIn const &)
        {
            --indent_;
        }

        void begin(AST::Program const &id)
        {
        }
        void end(AST::Program const &)
        {
            os << "\n";
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

} } } }

#endif // PRETTYPRINTER_HH_INCLUDED_20130821
