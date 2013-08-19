// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef ASTDUMPER_HH_INCLUDED_20130817
#define ASTDUMPER_HH_INCLUDED_20130817

#include "AST.hh"
#include <iostream>

namespace excyrender { namespace Nature { namespace Et1 {

    struct ASTDumper final : AST::Visitor {

        void begin(AST::Addition const &)
        {
            indent(); os << "(+) {\n";
            ++indent_;
        }
        void end(AST::Addition const &)
        {
            --indent_;
            indent(); os << "}\n";
        }

        void begin(AST::Subtraction const &)
        {
            indent(); os << "(-) {\n";
            ++indent_;
        }
        void end(AST::Subtraction const &)
        {
            --indent_;
            indent(); os << "}\n";
        }

        void begin(AST::Negation const &)
        {
            indent(); os << "(neg) {\n";
            ++indent_;
        }
        void end(AST::Negation const &)
        {
            --indent_;
            indent(); os << "}\n";
        }

        void begin(AST::Multiplication const &)
        {
            indent(); os << "(*) {\n";
            ++indent_;
        }
        void end(AST::Multiplication const &)
        {
            --indent_;
            indent(); os << "}\n";
        }

        void begin(AST::Division const &)
        {
            indent(); os << "(/) {\n";
            ++indent_;
        }
        void end(AST::Division const &)
        {
            --indent_;
            indent(); os << "}\n";
        }

        void begin(AST::IntegerLiteral const &lit)
        {
            indent(); os << (string)(*lit.from()) << "\n";
            ++indent_;
        }
        void end(AST::IntegerLiteral const &)
        {
            --indent_;
        }

        void begin(AST::Call const &call)
        {
            indent(); os << "call " << call.id() << "{\n";
            ++indent_;
        }
        void end(AST::Call const &)
        {
            --indent_;
            indent(); os << "}\n";
        }

        void begin(AST::ParenExpression const &call)
        {
            //indent(); os << "() {\n";
            //++indent_;
        }
        void end(AST::ParenExpression const &)
        {
            //--indent_;
            //indent(); os << "}\n";
        }

        void begin(AST::Binding const &binding)
        {
            indent(); os << "binding " << binding.id() << "\n";
            ++indent_;
            for (auto arg : binding.arguments()) {
                indent(); os << arg.name << " : " << arg.type << '\n';
            }
            --indent_;
            indent(); os << "{\n";
            ++indent_;
        }
        void end(AST::Binding const &)
        {
            --indent_;
            indent(); os << "}\n";
        }

        void begin(AST::Identifier const &id)
        {
            indent(); os << "id " << id.reference() << "\n";
            ++indent_;
        }
        void end(AST::Identifier const &)
        {
            --indent_;
        }

        void begin(AST::LetIn const &id)
        {
            ++indent_;
        }
        void end(AST::LetIn const &)
        {
            --indent_;
        }

        void begin(AST::Program const &id)
        {
            indent(); os << "program " << "\n";
            ++indent_;
        }
        void end(AST::Program const &)
        {
            --indent_;
        }

    private:
        std::ostream &os = std::cout;
        int indent_ = 0;
        void indent() {
            for (int i=0; i!=indent_; ++i) {
                std::cout << "    ";
            }
        }
    };
} } }

#endif // ASTDUMPER_HH_INCLUDED_20130817
