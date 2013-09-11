// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef ASTALGORITHM_HH_INCLUDED_20130910
#define ASTALGORITHM_HH_INCLUDED_20130910

#include "AST.hh"

namespace excyrender { namespace Nature { namespace Et1 { namespace Algorithm {

    void transform_expressions (AST::ASTNode &ast, std::function<void (shared_ptr<AST::Expression>&)> t);

    namespace detail {
        template <typename T>
        struct ForEach : AST::Transform {
            ForEach(std::function<void (T&)> repl)
                : fun_(repl) {}

        private:
            std::function<void (T&)> fun_;

            template <typename Other>
            void fun (Other &) {}

            void fun (T &n) { fun_(n); }

        public:
            void begin(AST::Addition &n) { fun(n); }
            void end(AST::Addition &) {}

            void begin(AST::Subtraction &n) { fun(n); }
            void end(AST::Subtraction &) {}

            void begin(AST::Multiplication &n) { fun(n); }
            void end(AST::Multiplication &) {}

            void begin(AST::Division &n) { fun(n); }
            void end(AST::Division &) {}

            void begin(AST::LessThan &n) { fun(n); }
            void end(AST::LessThan &) {}

            void begin(AST::LessEqual &n) { fun(n); }
            void end(AST::LessEqual &) {}

            void begin(AST::GreaterThan &n) { fun(n); }
            void end(AST::GreaterThan &) {}

            void begin(AST::GreaterEqual &n) { fun(n); }
            void end(AST::GreaterEqual &) {}

            void begin(AST::Equal &n) { fun(n); }
            void end(AST::Equal &) {}

            void begin(AST::NotEqual &n) { fun(n); }
            void end(AST::NotEqual &) {}

            void begin(AST::LogicalAnd &n) { fun(n); }
            void end(AST::LogicalAnd &) {}

            void begin(AST::LogicalOr &n) { fun(n); }
            void end(AST::LogicalOr &) {}

            void begin(AST::LogicalNot &n) { fun(n); }
            void end(AST::LogicalNot &) {}

            void transform(AST::IntegerLiteral &n) { fun(n); }
            void transform(AST::RealLiteral &n) { fun(n); }
            void transform(AST::BoolLiteral &n) { fun(n); }
            void transform(AST::Identifier &n) { fun(n); }

            void begin(AST::Call &n) { fun(n); }
            void end(AST::Call &) {}

            void begin(AST::Negation &n) { fun(n); }
            void end(AST::Negation &) {}

            void begin(AST::ParenExpression &n) { fun(n); }
            void end(AST::ParenExpression &) {}

            void begin(AST::Binding &n) { fun(n); }
            void end(AST::Binding &) {}

            void begin(AST::IfThenElse &n) { fun(n); }
            void end(AST::IfThenElse &) {}

            void begin(AST::LetIn &n) { fun(n); }
            void end(AST::LetIn &) {}

            void begin(AST::Program &n) { fun(n); }
            void end(AST::Program &) {}

        private:
        };

        template <typename T>
        struct ForEachAtEnd : AST::Transform {
            ForEachAtEnd(std::function<void (T&)> repl)
                : fun_(repl) {}

        private:
            std::function<void (T&)> fun_;

            template <typename Other>
            void fun (Other &) {}

            void fun (T &n) { fun_(n); }

        public:
            void begin(AST::Addition&) {}
            void end(AST::Addition& n) { fun(n); }

            void begin(AST::Subtraction&) {}
            void end(AST::Subtraction& n) { fun(n); }

            void begin(AST::Multiplication&) {}
            void end(AST::Multiplication& n) { fun(n); }

            void begin(AST::Division&) {}
            void end(AST::Division& n) { fun(n); }

            void begin(AST::LessThan&) {}
            void end(AST::LessThan& n) { fun(n); }

            void begin(AST::LessEqual&) {}
            void end(AST::LessEqual& n) { fun(n); }

            void begin(AST::GreaterThan&) {}
            void end(AST::GreaterThan& n) { fun(n); }

            void begin(AST::GreaterEqual&) {}
            void end(AST::GreaterEqual& n) { fun(n); }

            void begin(AST::Equal&) {}
            void end(AST::Equal& n) { fun(n); }

            void begin(AST::NotEqual&) {}
            void end(AST::NotEqual& n) { fun(n); }

            void begin(AST::LogicalAnd&) {}
            void end(AST::LogicalAnd& n) { fun(n); }

            void begin(AST::LogicalOr&) {}
            void end(AST::LogicalOr& n) { fun(n); }

            void begin(AST::LogicalNot&) {}
            void end(AST::LogicalNot& n) { fun(n); }

            void transform(AST::IntegerLiteral &n) { fun(n); }
            void transform(AST::RealLiteral &n) { fun(n); }
            void transform(AST::BoolLiteral &n) { fun(n); }
            void transform(AST::Identifier &n) { fun(n); }

            void begin(AST::Call&) {}
            void end(AST::Call& n) { fun(n); }

            void begin(AST::Negation&) {}
            void end(AST::Negation& n) { fun(n); }

            void begin(AST::ParenExpression&) {}
            void end(AST::ParenExpression& n) { fun(n); }

            void begin(AST::Binding&) {}
            void end(AST::Binding& n) { fun(n); }

            void begin(AST::IfThenElse&) {}
            void end(AST::IfThenElse& n) { fun(n); }

            void begin(AST::LetIn&) {}
            void end(AST::LetIn& n) { fun(n); }

            void begin(AST::Program&) {}
            void end(AST::Program& n) { fun(n); }

        private:
        };
    }

    template <typename T>
    void for_each (AST::ASTNode &ast, std::function<void (T&)> t)
    {
        detail::ForEach<T> te(t);
        ast.accept(te);
    }

    template <typename T>
    void for_each_at_end (AST::ASTNode &ast, std::function<void (T&)> t)
    {
        detail::ForEachAtEnd<T> te(t);
        ast.accept(te);
    }

} } } }

#endif // ASTALGORITHM_HH_INCLUDED_20130910
