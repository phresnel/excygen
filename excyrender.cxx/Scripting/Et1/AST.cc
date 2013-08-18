// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "AST.hh"
#include "ASTDumper.hh"
#include "optional.hh"
#include <map>
#include <set>
#include <vector>
#include <stdexcept>
#include <iostream>

namespace excyrender { namespace Nature { namespace Et1 { namespace {

    using AST::Argument;
    using AST::Address;
    using std::vector;

    class Scope {
    public:
        Scope() = default;
        Scope(Scope &&) = default;
        Scope& operator= (Scope &&) = default;

        Scope(Scope const &) = delete;
        Scope& operator= (Scope const &) = delete;


        void declare_argument(string type, string name) {
            addresses.push_back(Address::Index(type, name, index++));
        }

        Scope localize() const {
            std::cout << "Scope::localize()" << std::endl;
            Scope s;
            s.base_index = base_index + 1;

            for (auto adr : addresses) {
                if (adr.kind() == Address::StackIndex) {
                    s.addresses.push_back(adr.toBigIndex(base_index));
                } else {
                    s.addresses.push_back(adr);
                }
            }
            return s;
        }

        Scope copy() const {
            Scope s;
            s.base_index = base_index;
            s.addresses = addresses;
            s.index = index;
            return s;
        }

        Address lookup(string const &name) const {
            for (auto adr : addresses)
                if (adr.name() == name) return adr;
            return Address();
        }

    private:
        vector<Address> addresses;
        int index = 0;
        int base_index = 0;
    };


} } } }



namespace excyrender { namespace Nature { namespace Et1 { namespace {

    struct BinaryOperator {
        string symbol;
        int precedence;

        using Factory = std::function<shared_ptr<AST::Expression> (token_iter, token_iter, shared_ptr<AST::Expression>, shared_ptr<AST::Expression>)>;
        Factory create;

        template <typename NodeT>
        static Factory create_factory() {
            return [](token_iter a, token_iter b, shared_ptr<AST::Expression> c, shared_ptr<AST::Expression> d)
                    {
                        auto e = new NodeT(a,b,c,d);
                        return shared_ptr<AST::Expression>(e);
                    };
        }

        BinaryOperator() = delete;
        BinaryOperator (string const &symbol, int precedence, Factory create) :
            symbol(symbol), precedence(precedence), create(create) {}
    };
    std::map<string, BinaryOperator> initial_precedence_table() {
        std::map<string, BinaryOperator> ret;
        // TODO: re-check map::emplace() availability. So long, // 'insert' because deleted default ctor.

        ret.insert(std::make_pair("+", BinaryOperator{"+", 20, BinaryOperator::create_factory<AST::Addition>()}));
        ret.insert(std::make_pair("-", BinaryOperator{"-", 20, BinaryOperator::create_factory<AST::Subtraction>()}));
        ret.insert(std::make_pair("*", BinaryOperator{"*", 40, BinaryOperator::create_factory<AST::Multiplication>()}));
        ret.insert(std::make_pair("/", BinaryOperator{"/", 40, BinaryOperator::create_factory<AST::Division>()}));
        return ret;
    }
    std::map<string, BinaryOperator> precedence_table = initial_precedence_table(); // TODO: not as a global but as a parameter, which may enable operator overloading.
    int precedence (string const &op, std::map<string, BinaryOperator>::iterator *where = nullptr) {
        const auto it = precedence_table.find(op);
        if (where)
            *where = it;
        if (it != precedence_table.end())
            return it->second.precedence;
        return -1;
    }


    shared_ptr<AST::Expression>     expression(token_iter, token_iter, Scope const &);
    shared_ptr<AST::IntegerLiteral> integer_literal(token_iter, token_iter);
    shared_ptr<AST::Terminal>       terminal (token_iter it, token_iter end, Scope const &);


    // function-call : identifier '(' expression ')'
    shared_ptr<AST::Call> call(token_iter it, token_iter end, Scope const &scope)
    {
        using namespace AST;

        const token_iter call_begin = it;

        // some-call ( foo , bar )
        // ^^^^^^^^^
        if (it->kind != TokenKind::Identifier)
            return shared_ptr<Call>();
        const string callee = *it;
        ++it;

        // some-call ( foo , bar )
        //           ^
        if (it == end || it->kind != LParen)
            return shared_ptr<Call>();
        ++it;

        // Arguments.
        std::vector<shared_ptr<Expression>> arguments;

        if (it->kind!=RParen) {
            // some-call ( foo , bar )
            //             ^^^  OR   ^
            while (it!=end) {
                // some-call ( foo , bar )
                //             ^^^
                {
                    auto arg = expression(it, end, scope);
                    if (!arg)
                        throw std::runtime_error("expected argument");
                    it = arg->to();
                    arguments.push_back(arg);
                }

                // some-call ( foo , bar )
                //                 ^ OR  ^
                if (it == end || it->kind == RParen)
                    break;
                if (it->kind != Comma)
                    throw std::runtime_error("expected ','");
                ++it;
            }
        }

        if (it == end || it->kind != RParen)
            throw std::runtime_error("unclosed '(' in function call");
        ++it;
        return shared_ptr<Call>(new Call(call_begin, it, callee, arguments));
    }



    // binding : name ( '(' argument (',' argument)* ')' )? = expression
    // argument: type? name
    shared_ptr<AST::Binding> binding(token_iter it, token_iter end, Scope const &scope_)
    {
        const auto start = it;
        // name ( '(' argument (',' argument)* ')' )? = expression
        // ^
        if (it->kind != Identifier)
            return shared_ptr<AST::Binding>();
        const string name = *it;
        ++it;

        if (it == end)
            return shared_ptr<AST::Binding>();

        Scope scope = scope_.localize();

        vector<AST::Argument> arguments;
        if (it->kind == LParen) {
            // name ( '(' argument (',' argument)* ')' )? = expression
            //         ^
            ++it;

            if (it!=end && it->kind != RParen) {
                while (it!=end) {
                    if (it->kind != TokenKind::Identifier) // This can't be a binding, then.
                        return shared_ptr<AST::Binding>();

                    const auto arg = AST::Argument("any", *it, arguments.size());
                    arguments.push_back(arg);
                    scope.declare_argument(arg.type, arg.name);

                    ++it;
                    if (it == end)
                        return shared_ptr<AST::Binding>();//throw std::runtime_error("expected ',' or ')' got EOF");
                    if (it->kind == RParen) {
                        break;
                    }
                    if (it->kind != Comma)
                        return shared_ptr<AST::Binding>();//throw std::runtime_error("expected ',' or ')', got " + string(*it));
                    ++it;
                }
            }

            if (it->kind != RParen)
                throw std::runtime_error("expected ')'");
            ++it;
        }

        // name ( '(' argument (',' argument)* ')' )? = expression
        //                                            ^
        if (it==end || it->kind!=Equal)
            return shared_ptr<AST::Binding>();

        ++it;
        if (it == end)
            throw std::runtime_error("expected binding expression");
        auto e = expression(it, end, scope);
        if (!e)
            throw std::runtime_error("expected binding expression");

        return shared_ptr<AST::Binding>(new AST::Binding(start, e->to(),
                                                         name, arguments, e));
    }



    shared_ptr<AST::IntegerLiteral> integer_literal(token_iter it, token_iter end)
    {
        using namespace AST;

        if (it->kind != Integer) return shared_ptr<IntegerLiteral>();
        return shared_ptr<IntegerLiteral>(new IntegerLiteral(it, it+1));
    }



    shared_ptr<AST::Identifier> identifier(token_iter it, token_iter end, Scope const &scope)
    {
        using namespace AST;

        if (it->kind != TokenKind::Identifier)
            return shared_ptr<AST::Identifier>();

        auto address = scope.lookup(*it);
        return shared_ptr<AST::Identifier>(new AST::Identifier(it, it+1, *it, address));
    }



    shared_ptr<AST::Unary> unary(token_iter it, token_iter end, Scope const &scope)
    {
        using namespace AST;

        if (it->kind == Minus) {
            auto rhs = terminal(it+1, end, scope);
            if (!rhs)
                throw std::runtime_error("expected argument to negation-operator");
            return shared_ptr<Negation>(new Negation(it, rhs->to(), rhs));
        }

        return shared_ptr<AST::Unary>();
    }



    shared_ptr<AST::Terminal> terminal (token_iter it, token_iter end, Scope const &scope_)
    {
        if (auto e = binding(it, end, scope_))
            return e;
        if (auto e = call(it, end, scope_))
            return e;
        if (auto e = identifier(it, end, scope_))
            return e;
        if (auto e = integer_literal(it, end))
            return e;
        if (auto e = unary(it, end, scope_))
            return e;
        if (it->kind == LParen) {
            if (auto e = expression(it+1, end, scope_)) {
                if (e->to()->kind != RParen)
                    throw std::runtime_error("missing ')'");
                return shared_ptr<AST::ParenExpression>(
                            new AST::ParenExpression(it, e->to()+1, e));
            }
        }
        if (it->kind == Let) {
            const auto start = it;
            vector<shared_ptr<AST::Binding>> bindings;

            Scope scope = scope_.copy();

            ++it;
            while (it != end) {
                auto e = binding(it, end, scope);
                if (!e)
                    throw std::runtime_error("only bindings allowed within 'let/in'-sequence");
                it = e->to();
                bindings.push_back(e);
                scope.declare_argument("any", e->id());

                if (it==end)
                    break;
                if (it->kind == In)
                    break;
                if (it->kind == Comma) {
                    ++it;
                    continue;
                }

                throw std::runtime_error("expected 'in' or ',', got '" + string(*it) + "'");
            }

            if (it==end || it->kind!=In)
                throw std::runtime_error("missing 'in' after 'let'");
            ++it;
            if (it == end)
                throw std::runtime_error("missing value expression after 'in'");
            auto value = expression(it, end, scope);
            if (!value)
                throw std::runtime_error("missing value expression after 'in'");
            it = value->to();

            return shared_ptr<AST::LetIn>(new AST::LetIn(start, it, bindings, value));
        }
        return shared_ptr<AST::Terminal>();
    }



    shared_ptr<AST::Expression> binary(int min_prec, shared_ptr<AST::Expression> lhs,
                                       token_iter it, token_iter end, Scope const &scope)
    {
        using namespace AST;

        while (1) {
            if (it == end)
                return lhs;

            // 1 + 2 + 3
            //   ^
            std::map<string, BinaryOperator>::iterator prec_entry;
            const int prec = precedence(string(*it), &prec_entry);
            if (prec < min_prec) {
                return lhs;
            }
            ++it;

            // 1 + 2 + 3
            //     ^
            shared_ptr<AST::Expression> rhs = terminal(it, end, scope);
            if (!rhs)
                throw std::runtime_error("expected operand on right-hand-side of operator");
            it = rhs->to();

            // 1 + 2 ? 3
            //       ^
            const int next_prec = precedence(string(*it));
            if (prec < next_prec) {
                // This means '?' has higher precedence (e.g. '*').
                rhs = binary(next_prec, rhs, it, end, scope); // Therefore, what is our rhs should really be ?'s lhs.
                if (!rhs)
                    return shared_ptr<AST::Expression>();
                it = rhs->to();
            }

            lhs = prec_entry->second.create(lhs->from(), rhs->to(), lhs, rhs);
        }
    }



    shared_ptr<AST::Expression> expression(token_iter it, token_iter end, Scope const &scope)
    {
        using namespace AST;

        auto lhs = terminal(it, end, scope);
        if (!lhs) {
            std::clog << "no terminal found" << std::endl;
            return shared_ptr<Expression>();
        }
        it = lhs->to();
        return binary(0, lhs, it, end, scope);
    }


    shared_ptr<AST::Program> program(token_iter it, token_iter end, Scope const &scope) {
        // TODO: extract the following as "bindings(it,end, Static, Dynamic)", same for "LetIn"
        if (it->kind == Static) {
            const auto start = it;
            vector<shared_ptr<AST::Binding>> bindings;

            ++it;
            while (it != end) {
                auto e = binding(it, end, scope);
                if (!e)
                    throw std::runtime_error("only bindings allowed within 'static/dynamic'-sequence");
                it = e->to();
                bindings.push_back(e);

                if (it==end)
                    break;
                if (it->kind == Dynamic)
                    break;
                if (it->kind == Comma) {
                    ++it;
                    continue;
                }

                throw std::runtime_error("expected 'in' or ',', got '" + string(*it) + "'");
            }

            if (it==end || it->kind!=Dynamic)
                throw std::runtime_error("missing 'dynamic' after 'static'");
            ++it;
            if (it == end)
                throw std::runtime_error("missing value expression after 'dynamic'");
            auto value = expression(it, end, scope);
            if (!value)
                throw std::runtime_error("missing value expression after 'dynamic'");
            it = value->to();

            return shared_ptr<AST::Program>(new AST::Program(start, it, bindings, value));
        } else if (auto e = expression(it, end, scope)) {
            return shared_ptr<AST::Program>(
                      new AST::Program(it, end,
                                       vector<shared_ptr<AST::Binding>>(),
                                       e));
        }

        return shared_ptr<AST::Program>();
    }


    HeightFunction compile (vector<Token> const &toks) {
        if (toks.empty())
            throw std::runtime_error("no tokens");
        std::cout << toks << std::endl;

        Scope scope;
        shared_ptr<AST::Program> prog = program(toks.begin(), toks.end(), scope);

        ASTDumper dumper;
        prog->accept(dumper);

        throw std::runtime_error("no expression found");
    }

} } } }



// -- API ------------------------------------------------------------------------------------------
namespace excyrender { namespace Nature { namespace Et1 {

HeightFunction compile (std::string const &code) {
    return compile(tokenize("bar(x,y,z) = let b=0, bar(a)=let Y=a*b+z in a+Y in x+y+z"));
    /*
       "static \n"
       "  x = 3*2*1 \n"
       "dynamic \n"
       "fac (x) = let foo(x) = (5+x), bar(y) = let f(y)=y*2 in f(y)  \n"
       "          in bar(3)"));
    */
}

} } }
