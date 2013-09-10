// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "ASTAlgorithm.hh"

namespace excyrender { namespace Nature { namespace Et1 { namespace Algorithm {

    struct TransformExpressions : AST::Transform {
        TransformExpressions(std::function<void (shared_ptr<AST::Expression>&)> repl)
        : replace(repl) {}

    private:
        std::function<void (shared_ptr<AST::Expression>&)> replace;

    public:
        void begin(AST::Addition &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::Addition &ast) {}

        void begin(AST::Subtraction &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::Subtraction &ast) {}

        void begin(AST::Multiplication &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::Multiplication &ast) {}

        void begin(AST::Division &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::Division &ast) {}

        void begin(AST::LessThan &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::LessThan &ast) {}

        void begin(AST::LessEqual &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::LessEqual &ast) {}

        void begin(AST::GreaterThan &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::GreaterThan &ast) {}

        void begin(AST::GreaterEqual &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::GreaterEqual &ast) {}

        void begin(AST::Equal &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::Equal &ast) {}

        void begin(AST::NotEqual &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::NotEqual &ast) {}

        void begin(AST::LogicalAnd &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::LogicalAnd &ast) {}

        void begin(AST::LogicalOr &ast) {
            replace(ast.lhs_ptr());
            replace(ast.rhs_ptr());
        }
        void end(AST::LogicalOr &ast) {}

        void begin(AST::LogicalNot &ast) {
            replace(ast.rhs_ptr());
        }
        void end(AST::LogicalNot &) {}

        void transform(AST::IntegerLiteral &) {}
        void transform(AST::RealLiteral &) {}
        void transform(AST::BoolLiteral &) {}
        void transform(AST::Identifier &) {}

        void begin(AST::Call &call) {
            for (auto &ptr : call.arguments())
                replace(ptr);
        }
        void end(AST::Call &) {}

        void begin(AST::Negation &ast) {
            replace(ast.rhs_ptr());
        }
        void end(AST::Negation &) {}

        void begin(AST::ParenExpression &ast) {
            replace(ast.expression_ptr());
        }
        void end(AST::ParenExpression &) {}

        void begin(AST::Binding &b) {
            replace(b.body_ptr());
        }
        void end(AST::Binding &) {}

        void begin(AST::IfThenElse &b) {
            replace(b.condition_ptr());
            replace(b.thenExpression_ptr());
            replace(b.elseExpression_ptr());
        }
        void end(AST::IfThenElse &) {}

        void begin(AST::LetIn &b) {
            replace(b.value_ptr());
            // the list of bindings must remain a list of bindings, so not expanding on them here.
        }
        void end(AST::LetIn &) {}

        void begin(AST::Program &b) {
            replace(b.value_ptr());
            // the list of bindings must remain a list of bindings, so not expanding on them here.
        }
        void end(AST::Program &) {}

    private:
    };

    void transform_expressions (AST::ASTNode &ast, std::function<void (shared_ptr<AST::Expression>&)> t)
    {
        TransformExpressions te(t);
        ast.accept(te);
    }

} } } }

