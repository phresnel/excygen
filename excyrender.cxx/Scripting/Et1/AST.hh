// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef AST_HH_INCLUDED_20130816
#define AST_HH_INCLUDED_20130816

#include "Token.hh"
#include "Nature/HeightFunction.hh"
#include "memory.hh"


// -- Compilation ----------------------------------------------------------------------------------
namespace excyrender { namespace Nature { namespace Et1 { namespace AST {

    class ASTNode {
    public:
        ASTNode() = delete;
        virtual ~ASTNode() {}

        token_iter from() const noexcept { return from_; }
        token_iter to()   const noexcept { return to_; }

    protected:
        ASTNode(token_iter from, token_iter to) : from_(from), to_(to) {}

    private:
        token_iter from_, to_;
    };

    struct Expression : ASTNode {
        Expression (token_iter from, token_iter to) : ASTNode(from, to) {}
        virtual ~Expression() {}
    };


    // -- Binary operations ------------------------------------------------------------------------
    struct Binary : Expression {
        virtual ~Binary() {}
        Expression const &lhs() const { return *lhs_; }
        Expression const &rhs() const { return *rhs_; }

    protected:
        Binary (token_iter from, token_iter to,
                shared_ptr<Expression> lhs, shared_ptr<Expression> rhs
               ) : Expression(from, to), lhs_(lhs), rhs_(rhs) {}

    private:
        shared_ptr<Expression> lhs_, rhs_;
    };

    struct Addition final : Binary {
        Addition (token_iter from, token_iter to,
                  shared_ptr<Expression> lhs, shared_ptr<Expression> rhs
                  ) : Binary(from, to, lhs, rhs)
        {}
    };

    struct Subtraction final : Binary {
        Subtraction (token_iter from, token_iter to,
                     shared_ptr<Expression> lhs, shared_ptr<Expression> rhs
                    ) : Binary(from, to, lhs, rhs)
        {}
    };

    struct Multiplication final : Binary {
        Multiplication (token_iter from, token_iter to,
                        shared_ptr<Expression> lhs, shared_ptr<Expression> rhs
                       ) : Binary(from, to, lhs, rhs)
        {}
    };

    struct Division final : Binary {
        Division (token_iter from, token_iter to,
                  shared_ptr<Expression> lhs, shared_ptr<Expression> rhs
                 ) : Binary(from, to, lhs, rhs)
        {}
    };


    // -- "End points" -----------------------------------------------------------------------------
    struct Terminal : Expression {
        Terminal (token_iter from, token_iter to) : Expression(from, to) {}
        virtual ~Terminal() {}
    };

    struct Literal : Terminal {
        Literal (token_iter from, token_iter to) : Terminal(from, to) {}
        virtual ~Literal() {}
    };

    struct IntegerLiteral final : Literal {
        IntegerLiteral (token_iter from, token_iter to) : Literal(from, to) {}
    };

    struct Call final : Terminal {
        Call (token_iter from, token_iter to,
              std::string const &id, vector<shared_ptr<Expression>> args)
        : Terminal(from, to), id_(id), arguments_(args)
        {}

        std::string id() const { return id_; }
        vector<shared_ptr<Expression>>::size_type      args_size()  const { return arguments_.size(); }
        vector<shared_ptr<Expression>>::const_iterator args_begin() const { return arguments_.begin(); }
        vector<shared_ptr<Expression>>::const_iterator args_end()   const { return arguments_.end(); }
    private:
        std::string id_;
        vector<shared_ptr<Expression>> arguments_;
    };

} } } }



namespace excyrender { namespace Nature { namespace Et1 {

    HeightFunction compile (std::string const &code);

} } }



#endif // AST_HH_INCLUDED_20130816
