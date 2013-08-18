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

    struct Argument {
        string type;
        string name;

        Argument(string type, string name) : type(type), name(name) {}
    };

    class Addition;
    class Subtraction;
    class Multiplication;
    class Division;
    class IntegerLiteral;
    class Call;
    class Negation;
    class ParenExpression;
    class Binding;
    class Identifier;
    class LetIn;

    struct Visitor {
        virtual void begin(Addition const &) = 0;
        virtual void end(Addition const &) = 0;

        virtual void begin(Subtraction const &) = 0;
        virtual void end(Subtraction const &) = 0;

        virtual void begin(Multiplication const &) = 0;
        virtual void end(Multiplication const &) = 0;

        virtual void begin(Division const &) = 0;
        virtual void end(Division const &) = 0;

        virtual void begin(IntegerLiteral const &) = 0;
        virtual void end(IntegerLiteral const &) = 0;

        virtual void begin(Call const &) = 0;
        virtual void end(Call const &) = 0;

        virtual void begin(Negation const &) = 0;
        virtual void end(Negation const &) = 0;

        virtual void begin(ParenExpression const &) = 0;
        virtual void end(ParenExpression const &) = 0;

        virtual void begin(Binding const &) = 0;
        virtual void end(Binding const &) = 0;

        virtual void begin(Identifier const &) = 0;
        virtual void end(Identifier const &) = 0;

        virtual void begin(LetIn const &) = 0;
        virtual void end(LetIn const &) = 0;
    };


    class ASTNode {
    public:
        ASTNode() = delete;
        virtual ~ASTNode() {}

        token_iter from() const noexcept { return from_; }
        token_iter to()   const noexcept { return to_; }

        virtual void accept(Visitor &v) const = 0;
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

        void accept(Visitor &v) const {
            v.begin(*this);
            lhs().accept(v);
            rhs().accept(v);
            v.end(*this);
        }
    };

    struct Subtraction final : Binary {
        Subtraction (token_iter from, token_iter to,
                     shared_ptr<Expression> lhs, shared_ptr<Expression> rhs
                    ) : Binary(from, to, lhs, rhs)
        {}

        void accept(Visitor &v) const {
            v.begin(*this);
            lhs().accept(v);
            rhs().accept(v);
            v.end(*this);
        }
    };

    struct Multiplication final : Binary {
        Multiplication (token_iter from, token_iter to,
                        shared_ptr<Expression> lhs, shared_ptr<Expression> rhs
                       ) : Binary(from, to, lhs, rhs)
        {}

        void accept(Visitor &v) const {
            v.begin(*this);
            lhs().accept(v);
            rhs().accept(v);
            v.end(*this);
        }
    };

    struct Division final : Binary {
        Division (token_iter from, token_iter to,
                  shared_ptr<Expression> lhs, shared_ptr<Expression> rhs
                 ) : Binary(from, to, lhs, rhs)
        {}

        void accept(Visitor &v) const {
            v.begin(*this);
            lhs().accept(v);
            rhs().accept(v);
            v.end(*this);
        }
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

        void accept(Visitor &v) const {
            v.begin(*this);
            v.end(*this);
        }
    };

    struct Identifier final : Terminal {
        Identifier (token_iter from, token_iter to, string name) : Terminal(from, to), name(name) {}
        virtual ~Identifier() {}

        string id() const { return name; }

        void accept(Visitor &v) const {
            v.begin(*this);
            v.end(*this);
        }

    private:
        string name;
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

        void accept(Visitor &v) const {
            v.begin(*this);
            for (auto arg : arguments_)
                arg->accept(v);
            v.end(*this);
        }

    private:
        std::string id_;
        vector<shared_ptr<Expression>> arguments_;
    };

    struct Binding final : Terminal {
        Binding(token_iter from, token_iter to,
                string id,
                vector<Argument> arguments,
                shared_ptr<Expression> body)
            : Terminal (from, to), id_(id), arguments_(arguments), body_(body)
        {}

        void accept(Visitor &v) const {
            v.begin(*this);
            body().accept(v);
            v.end(*this);
        }

        string id() const { return id_; }
        Expression const &body() const { return *body_; }
        vector<Argument> const &arguments() const { return arguments_; }

    private:
        string id_;
        vector<Argument> arguments_;
        shared_ptr<Expression> body_;
    };

    struct LetIn final : Terminal {
        LetIn(token_iter from, token_iter to,
              vector<shared_ptr<Binding>> bindings,
              shared_ptr<Expression> value)
            : Terminal (from, to), bindings_(bindings), value_(value)
        {}

        void accept(Visitor &v) const {
            v.begin(*this);
            for (auto b : bindings_)
                b->accept(v);
            value().accept(v);
            v.end(*this);
        }

        Expression const &value() const { return *value_; }

    private:
        vector<shared_ptr<Binding>> bindings_;
        shared_ptr<Expression> value_;
    };


    // We need this as an extra class because the parser relies on the from() and to() functions,
    // which would be off-by-one if we'd just use Expression for expressions in parens.
    struct ParenExpression final : Terminal {
        ParenExpression(token_iter from, token_iter to,
                        shared_ptr<Expression> expression)
        : Terminal(from, to), expression_(expression)
        {
        }

        Expression const &expression() const { return *expression_; }

        void accept(Visitor &v) const {
            v.begin(*this);
            expression().accept(v);
            v.end(*this);
        }

    private:
        shared_ptr<Expression> expression_;
    };


    // -- Unary operations -------------------------------------------------------------------------
    struct Unary : Terminal {
        virtual ~Unary() {}
        Expression const &rhs() const { return *rhs_; }

    protected:
        Unary (token_iter from, token_iter to, shared_ptr<Expression> rhs) :
            Terminal(from, to), rhs_(rhs) {}

    private:
        shared_ptr<Expression> rhs_;
    };


    struct Negation final : Unary {
        Negation (token_iter from, token_iter to, shared_ptr<Expression> rhs) :
            Unary(from, to, rhs)
        {}

        void accept(Visitor &v) const {
            v.begin(*this);
            rhs().accept(v);
            v.end(*this);
        }
    };




} } } }



namespace excyrender { namespace Nature { namespace Et1 {

    HeightFunction compile (std::string const &code);

} } }



#endif // AST_HH_INCLUDED_20130816
