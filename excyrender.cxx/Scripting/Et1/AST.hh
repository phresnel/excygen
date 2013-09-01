// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef AST_HH_INCLUDED_20130816
#define AST_HH_INCLUDED_20130816

#include "Token.hh"
#include "Nature/HeightFunction.hh"
#include "memory.hh"
#include "optional.hh"
#include <stdexcept>


// -- Compilation ----------------------------------------------------------------------------------
namespace excyrender { namespace Nature { namespace Et1 { namespace AST {

    struct Typeinfo {
        Typeinfo() = default;
        Typeinfo (Typeinfo const &) = default;

        Typeinfo (Typeinfo &&ti) = default;
        Typeinfo& operator= (Typeinfo &&ti) = default;

        explicit Typeinfo(string const &name) : name_(name)
        {
            if (name.empty()) throw std::logic_error("empty typename");
            if (name[0] == '<') throw std::logic_error("passed raw type");
            if (name == "auto") name_.reset(); //throw std::logic_error("Typeinfo(string) used for 'auto'. Use Typeinfo() instead.");
        }

        Typeinfo& operator= (Typeinfo const &ti) {
            //if (!ti) throw std::logic_error("Typeinfo::operator: rhs is unresolved");
            if (*this) throw std::logic_error("Typeinfo::operator=: type was set already");
            name_ = ti.name_;
            return *this;
        }

        explicit operator bool () const { return name_; }

        // false <- any operand is unresolved
        // true  <- both operands resolved and equal
        bool operator== (Typeinfo const &ti) const {
            if (*this && ti) {
                return *name_ == *ti.name_;
            }
            return false;
        }

        bool operator!= (Typeinfo const &ti) const {
            return !(*this == ti);
        }

        string name() const {
            return name_ ? *name_ : "auto";
        }

    private:
        optional<string> name_;
    };

    struct Argument {
        Typeinfo type;
        string name;

        Argument(Typeinfo type, string name) : type(type), name(name) {}
    };

    class Addition;
    class Subtraction;
    class Multiplication;
    class Division;
    class IntegerLiteral;
    class RealLiteral;
    class Call;
    class Negation;
    class ParenExpression;
    class Binding;
    class Identifier;
    class LetIn;
    class Program;

    struct Visitor {
        virtual void begin(Addition const &) = 0;
        virtual void end(Addition const &) = 0;

        virtual void begin(Subtraction const &) = 0;
        virtual void end(Subtraction const &) = 0;

        virtual void begin(Multiplication const &) = 0;
        virtual void end(Multiplication const &) = 0;

        virtual void begin(Division const &) = 0;
        virtual void end(Division const &) = 0;

        virtual void infix() {}

        virtual void visit(IntegerLiteral const &) = 0;
        virtual void visit(RealLiteral const &) = 0;
        virtual void visit(Identifier const &) = 0;

        virtual void begin(Call const &) = 0;
        virtual void end(Call const &) = 0;

        virtual void begin(Negation const &) = 0;
        virtual void end(Negation const &) = 0;

        virtual void begin(ParenExpression const &) = 0;
        virtual void end(ParenExpression const &) = 0;

        virtual void begin(Binding const &) = 0;
        virtual void end(Binding const &) = 0;

        virtual void begin(LetIn const &) = 0;
        virtual void before_body(LetIn const &) {}
        virtual void end(LetIn const &) = 0;

        virtual void begin(Program const &) = 0;
        virtual void end(Program const &) = 0;
    };

    struct Transform {
        virtual void begin(Addition &) = 0;
        virtual void end(Addition &) = 0;

        virtual void begin(Subtraction &) = 0;
        virtual void end(Subtraction &) = 0;

        virtual void begin(Multiplication &) = 0;
        virtual void end(Multiplication &) = 0;

        virtual void begin(Division &) = 0;
        virtual void end(Division &) = 0;

        virtual void transform(IntegerLiteral &) = 0;
        virtual void transform(RealLiteral &) = 0;
        virtual void transform(Identifier &) = 0;

        virtual void begin(Call &) = 0;
        virtual void end(Call &) = 0;

        virtual void begin(Negation &) = 0;
        virtual void end(Negation &) = 0;

        virtual void begin(ParenExpression &) = 0;
        virtual void end(ParenExpression &) = 0;

        virtual void begin(Binding &) = 0;
        virtual void end(Binding &) = 0;

        virtual void begin(LetIn &) = 0;
        virtual void end(LetIn &) = 0;

        virtual void begin(Program &) = 0;
        virtual void end(Program &) = 0;
    };


    class ASTNode {
    public:
        ASTNode() = delete;
        virtual ~ASTNode() {}

        token_iter from() const noexcept { return from_; }
        token_iter to()   const noexcept { return to_; }

        virtual void accept(Visitor &v) const = 0;
        virtual void accept(Transform &v) = 0;

        Typeinfo type() const { return type_; }
        void reset_type(Typeinfo const &t) {
            type_ = t;
        }

        virtual ASTNode* deep_copy() const = 0;
    protected:
        ASTNode(token_iter from, token_iter to) : from_(from), to_(to) {}
        ASTNode(token_iter from, token_iter to, Typeinfo type) : from_(from), to_(to)
        {
            reset_type(type);
        }

    private:
        token_iter from_, to_;
        Typeinfo type_;
    };

    struct Expression : ASTNode {
        virtual Expression* deep_copy() const = 0;
        virtual ~Expression() {}
    protected:
        Expression (token_iter from, token_iter to) : ASTNode(from, to) {}
        Expression (token_iter from, token_iter to, Typeinfo type) : ASTNode(from, to, type) {}
    };


    // -- Binary operations ------------------------------------------------------------------------
    struct Binary : Expression {
        virtual ~Binary() {}
        Expression const &lhs() const { return *lhs_; }
        Expression const &rhs() const { return *rhs_; }
        Expression &lhs() { return *lhs_; }
        Expression &rhs() { return *rhs_; }

        void reset_lhs(Expression *e) {
            if (!e) throw std::logic_error("Binary::reset_lhs with nullptr");
            lhs_.reset(e);
        }
        void reset_rhs(Expression *e) {
            if (!e) throw std::logic_error("Binary::reset_rhs with nullptr");
            rhs_.reset(e);
        }
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
            v.infix();
            rhs().accept(v);
            v.end(*this);
        }

        void accept(Transform &v) {
            v.begin(*this);
            lhs().accept(v);
            rhs().accept(v);
            v.end(*this);
        }

        Addition* deep_copy() const {
            return new Addition(from(), to(),
                                shared_ptr<Expression>(lhs().deep_copy()),
                                shared_ptr<Expression>(rhs().deep_copy()));
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
            v.infix();
            rhs().accept(v);
            v.end(*this);
        }
        void accept(Transform &v) {
            v.begin(*this);
            lhs().accept(v);
            rhs().accept(v);
            v.end(*this);
        }

        Subtraction* deep_copy() const {
            return new Subtraction(from(), to(),
                                shared_ptr<Expression>(lhs().deep_copy()),
                                shared_ptr<Expression>(rhs().deep_copy()));
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
            v.infix();
            rhs().accept(v);
            v.end(*this);
        }
        void accept(Transform &v) {
            v.begin(*this);
            lhs().accept(v);
            rhs().accept(v);
            v.end(*this);
        }

        Multiplication* deep_copy() const {
            return new Multiplication(from(), to(),
                                shared_ptr<Expression>(lhs().deep_copy()),
                                shared_ptr<Expression>(rhs().deep_copy()));
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
            v.infix();
            rhs().accept(v);
            v.end(*this);
        }
        void accept(Transform &v) {
            v.begin(*this);
            lhs().accept(v);
            rhs().accept(v);
            v.end(*this);
        }

        Division* deep_copy() const {
            return new Division(from(), to(),
                                shared_ptr<Expression>(lhs().deep_copy()),
                                shared_ptr<Expression>(rhs().deep_copy()));
        }
    };


    // -- "End points" -----------------------------------------------------------------------------
    struct Terminal : Expression {
        virtual ~Terminal() {}
    protected:
        Terminal (token_iter from, token_iter to) : Expression(from, to) {}
        Terminal (token_iter from, token_iter to, Typeinfo type) : Expression(from, to, type) {}
    };

    struct Literal : Terminal {
        virtual ~Literal() {}
        string value() const { return value_; }
    protected:
        Literal (token_iter from, token_iter to, string value) :
            Terminal(from, to), value_(value) {}
    private:
        string value_;
    };

    struct IntegerLiteral final : Literal {
        IntegerLiteral (token_iter from, token_iter to, string value) :
            Literal(from, to, value) {}

        void accept(Visitor &v) const {
            v.visit(*this);
        }
        void accept(Transform &v) {
            v.transform(*this);
        }

        IntegerLiteral *deep_copy() const {
            return new IntegerLiteral(from(), to(), value());
        }
    };

    struct RealLiteral final : Literal {
        RealLiteral (token_iter from, token_iter to, string value) :
            Literal(from, to, value) {}

        void accept(Visitor &v) const {
            v.visit(*this);
        }
        void accept(Transform &v) {
            v.transform(*this);
        }

        RealLiteral *deep_copy() const {
            return new RealLiteral(from(), to(), value());
        }
    };


    struct Reference : Terminal {
        virtual ~Reference() {}
        string id() const { return name; }
    protected:
        Reference (token_iter from, token_iter to, string name) : Terminal(from, to), name(name) {}
    private:
        string name;
    };

    struct Identifier final : Reference {
        Identifier (token_iter from, token_iter to, string name) :
            Reference(from, to, name) {}

        void accept(Visitor &v) const {
            v.visit(*this);
        }
        void accept(Transform &v) {
            v.transform(*this);
        }

        Identifier *deep_copy() const {
            return new Identifier(from(), to(), id());
        }
    };

    struct Call final : Reference {
        Call (token_iter from, token_iter to,
              std::string const &id, vector<shared_ptr<Expression>> const &args)
        : Reference(from, to, id), arguments_(args)
        {}

        vector<shared_ptr<Expression>> const &arguments() const { return arguments_; }
        vector<shared_ptr<Expression>> &arguments() { return arguments_; }

        void accept(Visitor &v) const {
            v.begin(*this);
            bool first = true;
            auto const cp = arguments_; // only work on the current snapshot
            for (auto arg : cp) {
                if (!first) v.infix();
                first = false;
                arg->accept(v);
            }
            v.end(*this);
        }
        void accept(Transform &v) {
            v.begin(*this);
            auto const cp = arguments_; // only work on the current snapshot
            for (auto arg : cp)
                arg->accept(v);
            v.end(*this);
        }

        Call *deep_copy() const {
            vector<shared_ptr<Expression>> args;
            for (auto e : arguments_)
                args.emplace_back(e->deep_copy());
            return new Call(from(), to(), id(), args);
        }

    private:
        vector<shared_ptr<Expression>> arguments_;
    };

    struct Binding final : Terminal {
        Binding(token_iter from, token_iter to,
                string id,
                Typeinfo type,
                vector<Argument> arguments,
                shared_ptr<Expression> body)
            : Terminal (from, to, type), id_(id), arguments_(arguments), body_(body)
        {}

        void accept(Visitor &v) const {
            v.begin(*this);
            body().accept(v);
            v.end(*this);
        }
        void accept(Transform &v) {
            v.begin(*this);
            body().accept(v);
            v.end(*this);
        }

        string id() const { return id_; }

        Expression const &body() const { return *body_; }
        Expression &body() { return *body_; }
        vector<Argument> const &arguments() const { return arguments_; }
        vector<Argument> &arguments() { return arguments_; }

        void reset_body(Expression *e) {
            if (!e) throw std::logic_error("Binding::reset_body with nullptr");
            body_.reset(e);
        }

        Binding* deep_copy() const {
            return new Binding(from(), to(), id(), type(), arguments(),
                               shared_ptr<Expression>(body_->deep_copy()));
        }

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
            bool first = true;
            auto const cp = bindings_; // only work on the current snapshot
            for (auto b : cp) {
                if (!first) v.infix();
                first = false;
                b->accept(v);
            }
            v.before_body(*this);
            value().accept(v);
            v.end(*this);
        }
        void accept(Transform &v) {
            v.begin(*this);

            auto const cp = bindings_; // only work on the current snapshot
            for (auto b : cp)
                b->accept(v);
            value().accept(v);
            v.end(*this);
        }

        Expression const &value() const { return *value_; }
        Expression &value() { return *value_; }
        void reset_value(Expression *e) {
            if (!e) throw std::logic_error("LetIn::reset_value with nullptr");
            value_.reset(e);
        }

        vector<shared_ptr<Binding>> &bindings() { return bindings_; }
        vector<shared_ptr<Binding>> const &bindings() const { return bindings_; }

        LetIn *deep_copy() const {
            vector<shared_ptr<Binding>> bindings;
            for (auto b : bindings_)
                bindings.emplace_back(b->deep_copy());
            return new LetIn(from(), to(), bindings, shared_ptr<Expression>(value_->deep_copy()));
        }
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
        Expression &expression() { return *expression_; }

        void accept(Visitor &v) const {
            v.begin(*this);
            expression().accept(v);
            v.end(*this);
        }
        void accept(Transform &v) {
            v.begin(*this);
            expression().accept(v);
            v.end(*this);
        }

        ParenExpression *deep_copy() const {
            return new ParenExpression(from(), to(),
                                       shared_ptr<Expression>(expression_->deep_copy()));
        }

    private:
        shared_ptr<Expression> expression_;
    };


    // -- Unary operations -------------------------------------------------------------------------
    struct Unary : Terminal {
        virtual ~Unary() {}
        Expression const &rhs() const { return *rhs_; }
        Expression &rhs() { return *rhs_; }

        void reset_rhs(Expression *e) {
            if (!e) throw std::logic_error("Unary::reset_rhs with nullptr");
            rhs_.reset(e);
        }
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
        void accept(Transform &v) {
            v.begin(*this);
            rhs().accept(v);
            v.end(*this);
        }

        Negation* deep_copy() const {
            return new Negation(from(), to(), shared_ptr<Expression>(rhs().deep_copy()));
        }
    };

    // -- Unary operations -------------------------------------------------------------------------
    struct Program final : ASTNode {
        Program(token_iter from, token_iter to,
                vector<shared_ptr<Binding>> static_bindings,
                shared_ptr<Expression> value)
            : ASTNode (from, to), static_bindings_(static_bindings), value_(value)
        {}

        void accept(Visitor &v) const {
            v.begin(*this);
            bool first = true;
            auto const cp = static_bindings_; // only work on the current snapshot
            for (auto b : cp) {
                if (!first) v.infix();
                first = false;
                b->accept(v);
            }
            value().accept(v);
            v.end(*this);
        }
        void accept(Transform &v) {
            v.begin(*this);
            auto const cp = static_bindings_; // only work on the current snapshot
            for (auto b : cp)
                b->accept(v);
            value().accept(v);
            v.end(*this);
        }

        Expression const &value() const { return *value_; }
        Expression &value() { return *value_; }

        void reset_value(Expression *e) {
            if (!e) throw std::logic_error("LetProgram::reset_value with nullptr");
            value_.reset(e);
        }

        vector<shared_ptr<Binding>> &bindings() { return static_bindings_; }
        vector<shared_ptr<Binding>> const &bindings() const { return static_bindings_; }

        Program* deep_copy() const {
            vector<shared_ptr<Binding>> static_bindings;
            for (auto b : static_bindings_)
                static_bindings.emplace_back(b->deep_copy());
            return new Program(from(), to(), static_bindings,
                               shared_ptr<Expression>(value().deep_copy()));
        }
    private:
        vector<shared_ptr<Binding>> static_bindings_;
        shared_ptr<Expression> value_;
    };

} } } }



namespace excyrender { namespace Nature { namespace Et1 { namespace AST {
    shared_ptr<AST::Program> program(token_iter it, token_iter end);
} } } }

namespace excyrender { namespace Nature { namespace Et1 {
    HeightFunction compile (std::string const &code);
} } }



#endif // AST_HH_INCLUDED_20130816
