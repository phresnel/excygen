// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "has_unresolved.hh"
#include <stack>


//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"

TEST_CASE( "Et1/ASTQueries/has_unresolved", "Unresolved finding" ) {
}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Nature { namespace Et1 { namespace ASTQueries {

namespace {

    struct HasUnresolved final : AST::Visitor {

        HasUnresolved() {
            within_generic.push(false);
        }

        void begin(AST::Addition const &ast) { check(ast); }
        void end(AST::Addition const &) {}
        void begin(AST::Subtraction const &ast) { check(ast); }
        void end(AST::Subtraction const &) {}
        void begin(AST::Multiplication const &ast) { check(ast); }
        void end(AST::Multiplication const &) {}
        void begin(AST::Division const &ast) { check(ast); }
        void end(AST::Division const &) {}

        void begin(AST::LessThan const &ast) { check(ast); }
        void end(AST::LessThan const &)  {}
        void begin(AST::LessEqual const &ast) { check(ast); }
        void end(AST::LessEqual const &)  {}
        void begin(AST::GreaterThan const &ast) { check(ast); }
        void end(AST::GreaterThan const &)  {}
        void begin(AST::GreaterEqual const &ast) { check(ast); }
        void end(AST::GreaterEqual const &)  {}
        void begin(AST::Equal const &ast) { check(ast); }
        void end(AST::Equal const &)  {}
        void begin(AST::NotEqual const &ast) { check(ast); }
        void end(AST::NotEqual const &) {}
        void begin(AST::LogicalAnd const &ast) { check(ast); }
        void end(AST::LogicalAnd const &) {}
        void begin(AST::LogicalOr const &ast) { check(ast); }
        void end(AST::LogicalOr const &) {}
        void begin(AST::LogicalNot const &ast) { check(ast); }
        void end(AST::LogicalNot const &) {}

        void begin(AST::IfThenElse const &ast) { check(ast); }
        void end(AST::IfThenElse const &) {}

        void visit(AST::IntegerLiteral const &ast) { check(ast); }
        void visit(AST::RealLiteral const &ast) { check(ast); }
        void visit(AST::BoolLiteral const &ast) { check(ast); }
        void visit(AST::Identifier const &ast) { check(ast); }

        void begin(AST::Call const &ast) { check(ast); }
        void end(AST::Call const &) {}

        void begin(AST::Negation const &ast) { check(ast); }
        void end(AST::Negation const &) {}

        void begin(AST::ParenExpression const &ast) { check(ast); }
        void end(AST::ParenExpression const &) {}

        void begin(AST::Binding const &binding) {
            within_generic.push(binding.is_generic());
            check(binding);
        }
        void end(AST::Binding const &) {
            within_generic.pop();
        }

        void begin(AST::LetIn const &ast) { check(ast); }
        void end(AST::LetIn const &) {}

        void begin(AST::Program const &ast) { check(ast); }
        void end(AST::Program const &) {}

        bool has_unresolved = false;

    private:

        void check(AST::ASTNode const &ast) {
            if (within_generic.top()) return;
            if (!ast.type()) has_unresolved = true;
        }

        std::stack<bool> within_generic;
    };

}


bool has_unresolved (shared_ptr<AST::ASTNode> ast) {
    if (!ast) return true;
    HasUnresolved fr;
    ast->accept(fr);
    return fr.has_unresolved;
}


bool has_unresolved (AST::ASTNode const &ast) {
    HasUnresolved fr;
    ast.accept(fr);
    return fr.has_unresolved;
}


} } } }
