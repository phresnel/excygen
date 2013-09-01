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

        void begin(AST::Addition const &) {}
        void end(AST::Addition const &) {}

        void begin(AST::Subtraction const &) {}
        void end(AST::Subtraction const &) {}

        void begin(AST::Multiplication const &) {}
        void end(AST::Multiplication const &) {}

        void begin(AST::Division const &) {}
        void end(AST::Division const &) {}

        void visit(AST::IntegerLiteral const &) {}
        void visit(AST::RealLiteral const &) {}
        void visit(AST::Identifier const &id) {
            if (within_generic.top()) return;
            if (!id.type()) {
                //std::cerr << "unresolved: " << id.id() << ":" << id.type().name() << std::endl;
                has_unresolved = true;
            }
        }

        void begin(AST::Call const &call) {
            if (within_generic.top()) return;
            if (!call.type()) has_unresolved = true;
        }
        void end(AST::Call const &) {}

        void begin(AST::Negation const &) {}
        void end(AST::Negation const &) {}

        void begin(AST::ParenExpression const &) {}
        void end(AST::ParenExpression const &) {}

        void begin(AST::Binding const &binding) {
            within_generic.push(binding.is_generic());
        }
        void end(AST::Binding const &) {
            within_generic.pop();
        }

        void begin(AST::LetIn const &letin) {}
        void end(AST::LetIn const &) {}

        void begin(AST::Program const &) {}
        void end(AST::Program const &) {}

        bool has_unresolved = false;

    private:

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
