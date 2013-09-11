// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "AST.hh"
#include "ASTPasses/1000_lambda_lift.hh"
#include "optional.hh"
#include <tuple>
#include <map>
#include <set>
#include <stdexcept>
#include <iostream>



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

        ret.insert(std::make_pair("*", BinaryOperator{"*", 320, BinaryOperator::create_factory<AST::Multiplication>()}));
        ret.insert(std::make_pair("/", BinaryOperator{"/", 320, BinaryOperator::create_factory<AST::Division>()}));

        ret.insert(std::make_pair("+", BinaryOperator{"+", 160, BinaryOperator::create_factory<AST::Addition>()}));
        ret.insert(std::make_pair("-", BinaryOperator{"-", 160, BinaryOperator::create_factory<AST::Subtraction>()}));

        ret.insert(std::make_pair("<", BinaryOperator{"<", 80, BinaryOperator::create_factory<AST::LessThan>()}));
        ret.insert(std::make_pair("<=", BinaryOperator{"<=", 80, BinaryOperator::create_factory<AST::LessEqual>()}));
        ret.insert(std::make_pair(">", BinaryOperator{">", 80, BinaryOperator::create_factory<AST::GreaterThan>()}));
        ret.insert(std::make_pair(">=", BinaryOperator{">=", 80, BinaryOperator::create_factory<AST::GreaterEqual>()}));

        ret.insert(std::make_pair("==", BinaryOperator{"==", 40, BinaryOperator::create_factory<AST::Equal>()}));
        ret.insert(std::make_pair("!=", BinaryOperator{"!=", 40, BinaryOperator::create_factory<AST::NotEqual>()}));

        ret.insert(std::make_pair("&&", BinaryOperator{"&&", 20, BinaryOperator::create_factory<AST::LogicalAnd>()}));
        ret.insert(std::make_pair("||", BinaryOperator{"||", 10, BinaryOperator::create_factory<AST::LogicalOr>()}));
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


    shared_ptr<AST::Expression>     expression(token_iter, token_iter);
    shared_ptr<AST::IntegerLiteral> integer_literal(token_iter, token_iter);
    shared_ptr<AST::Terminal>       terminal (token_iter it, token_iter end);


    // function-call : identifier '(' expression ')'
    shared_ptr<AST::Call> call(token_iter it, token_iter end)
    {
        using namespace AST;

        const token_iter call_begin = it;

        // some-call ( foo , bar )
        // ^^^^^^^^^
        if (it == end || it->kind != TokenKind::Identifier)
            return shared_ptr<Call>();
        const string callee = string(*it);
        ++it;

        // some-call ( foo , bar )
        //           ^
        if (it == end || it->kind != LParen)
            return shared_ptr<Call>();
        ++it;

        // Arguments.
        std::vector<shared_ptr<Expression>> arguments;

        if (it == end)
            throw std::runtime_error("missing ')'");
        if (it->kind!=RParen) {
            // some-call ( foo , bar )
            //             ^^^  OR   ^
            while (it!=end) {
                // some-call ( foo , bar )
                //             ^^^
                {
                    auto arg = expression(it, end);
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

    optional<AST::Argument> parse_signature (token_iter from, token_iter to)
    {
        // In this version, we only have 'int', 'float', 'double', 'auto', 'typeof(x)',
        // where in typeof, only the names of other arguments are allowed for now.
        auto const num_toks = distance(from, to);

        if (num_toks == 1) {
            return AST::Argument{AST::Typeinfo(), string(*from)};
        } else if (num_toks == 2) {
            return AST::Argument{AST::Typeinfo(string(*from)), string(*(from+1))};
        } else if (num_toks == 5
                   && *from == "typeof"
                   && (from+1)->kind == LParen
                   && (from+3)->kind == RParen)
        {
            return AST::Argument{AST::Typeinfo("typeof(" + string(*(from+2)) + ")"),
                                 string(*(from+4))};
        }
        return optional<AST::Argument>();
    }

    struct ParsedSig {
        token_iter from, to;
        AST::Typeinfo type;
        string name;
        ParsedSig(token_iter from, token_iter to,
                  AST::Typeinfo type, string name) : from(from), to(to), type(type), name(name)
        {}
    };

    optional<ParsedSig> typeof_sig (token_iter it, token_iter end)
    {
        const auto from = it;
        if (it == end || *it != "typeof")
            return optional<ParsedSig>();

        ++it;
        if (it==end || it->kind != LParen)
            throw std::runtime_error("expected '(' after 'typeof'");

        ++it;
        if (it==end || it->kind != Identifier)
            throw std::runtime_error("expected identifier inside 'typeof()'");
        const string of = string(*it);

        ++it;
        if (it==end || it->kind != RParen)
            throw std::runtime_error("expected ')' after 'typeof(...'");

        ++it;
        if (it==end || it->kind != Identifier)
            throw std::runtime_error("expected identifier after 'typeof()'");
        const string id = string(*it);

        ++it;
        return ParsedSig{from, it, AST::Typeinfo("typeof(" + of + ")"), id};
    }

    optional<ParsedSig> parse_sig (token_iter it, token_iter end)
    {
        if (it == end)
            return optional<ParsedSig>();

        /*
        if (distance(it, end) == 2 && string(*it) == "1" && string(*(1+it)) == ")") {
            std::cerr << "<<<<left:" << std::flush;
            for (auto a = it; a != end; ++a) {
                std::cerr << *a << std::flush;
            }
            std::cerr << ">>>>" << std::endl;
        }
        */

        if (auto t = typeof_sig(it, end))
            return t;

        if (it->kind == Identifier) {
            const auto from = it;
            ++it;
            if (it==end || it->kind != Identifier)
                return ParsedSig{from, it, AST::Typeinfo(), string(*from)};
            return ParsedSig{from, it+1, AST::Typeinfo(string(*from)), string(*it)};
        }
        return optional<ParsedSig>();
    }


    // binding : type? name ( '(' argument (',' argument)* ')' )? = expression
    // argument: type? name
    shared_ptr<AST::Binding> binding(token_iter it, token_iter end)
    {
        if (it == end)
            return shared_ptr<AST::Binding>();

        using std::vector;

        const auto start = it;
        // type? name ( '(' argument (',' argument)* ')' )? = expression
        // ^
        AST::Typeinfo type;
        string name;
        if (auto t = parse_sig(it,end)) {
            type = t->type;
            name = t->name;
            it = t->to;
        } else {
            return shared_ptr<AST::Binding>();
        }

        vector<AST::Argument> arguments;

        if (it == end)
            return shared_ptr<AST::Binding>();

        const bool has_argument_list = (it->kind == LParen);
        if (has_argument_list) {
            ++it;

            struct range { token_iter from, to;
                           range(token_iter from, token_iter to) : from(from), to(to) {}
                         };
            vector<range> args;

            // TODO: use parse_type() to dissect the argument list in-place.
            int nesting = 0;
            auto start_cur = it;
            while(1) {
                if (it==end) {
                    throw std::runtime_error("expected ',' or ')'");
                }
                else if (it->kind==RParen) {
                    if (nesting == 0) {
                        if (start_cur != it)
                            args.emplace_back(start_cur, it);
                        break;
                    }
                    --nesting;
                }
                else if (it->kind==LParen) {
                    ++nesting;
                }
                else if (it->kind == Comma) {
                    args.emplace_back(start_cur, it);
                    start_cur = it+1;
                }
                ++it;
            }

            // Now dissect the arguments.
            for (auto arg : args) {
                auto a = parse_signature(arg.from, arg.to);
                if (!a) return shared_ptr<AST::Binding>();
                arguments.push_back(*a);
            }

            ++it;
        }

        // name ( '(' argument (',' argument)* ')' )? = expression
        //                                            ^
        if (it==end || it->kind!=Equal)
            return shared_ptr<AST::Binding>();

        ++it;
        if (it == end)
            throw std::runtime_error("expected binding expression");
        auto e = expression(it, end);
        if (!e)
            throw std::runtime_error("expected binding expression");

        return shared_ptr<AST::Binding>(new AST::Binding(start, e->to(),
                                                         name, type, arguments, e,
                                                         has_argument_list ? AST::Binding::Function
                                                                           : AST::Binding::Value));
    }



    shared_ptr<AST::IntegerLiteral> integer_literal(token_iter it, token_iter end)
    {
        using namespace AST;

        if (it == end || it->kind != Integer)
            return shared_ptr<IntegerLiteral>();
        return shared_ptr<IntegerLiteral>(new IntegerLiteral(it, it+1, string(*it)));
    }

    shared_ptr<AST::RealLiteral> real_literal(token_iter it, token_iter end)
    {
        using namespace AST;

        if (it == end || it->kind != Real)
            return shared_ptr<RealLiteral>();
        return shared_ptr<RealLiteral>(new RealLiteral(it, it+1, string(*it)));
    }

    shared_ptr<AST::BoolLiteral> bool_literal(token_iter it, token_iter end)
    {
        using namespace AST;

        if (it == end || it->kind != Bool)
            return shared_ptr<BoolLiteral>();
        return shared_ptr<BoolLiteral>(new BoolLiteral(it, it+1, string(*it)));
    }



    shared_ptr<AST::Identifier> identifier(token_iter it, token_iter end)
    {
        using namespace AST;

        if (it == end || it->kind != TokenKind::Identifier)
            return shared_ptr<AST::Identifier>();
        return shared_ptr<AST::Identifier>(new AST::Identifier(it, it+1, string(*it)));
    }



    shared_ptr<AST::Unary> unary(token_iter it, token_iter end)
    {
        using namespace AST;
        if (it == end)
            return shared_ptr<AST::Unary>();

        if (it->kind == Minus) {
            auto rhs = terminal(it+1, end);
            if (!rhs)
                throw std::runtime_error("expected argument to negation-operator");
            return shared_ptr<Negation>(new Negation(it, rhs->to(), rhs));
        }
        if (it->kind == TokenKind::LogicalNot) {
            auto rhs = terminal(it+1, end);
            if (!rhs)
                throw std::runtime_error("expected argument to logical-not-operator");
            return shared_ptr<AST::LogicalNot>(new AST::LogicalNot(it, rhs->to(), rhs));
        }

        return shared_ptr<AST::Unary>();
    }



    shared_ptr<AST::IfThenElse> if_then_else(token_iter it, token_iter end)
    {
        const auto start = it;

        // <if> COND then EXPR else EXPR
        if (it == end || it->kind != TokenKind::If)
            return shared_ptr<AST::IfThenElse>();
        ++it;

        // if <COND> then EXPR else EXPR
        auto cond = expression(it, end);
        if (!cond)
            throw std::runtime_error("expected condition after 'if'");
        it = cond->to();

        // if COND <then> EXPR else EXPR
        if (it == end || it->kind != TokenKind::Then)
            throw std::runtime_error("expected 'then' after condition to 'if'");
        ++it;

        // if COND then <EXPR> else EXPR
        auto then_ = expression(it, end);
        if (!then_)
            throw std::runtime_error("expected expression after 'then'");
        it = then_->to();

        // if COND then EXPR <else> EXPR
        if (it == end || it->kind != TokenKind::Else)
            throw std::runtime_error("expected 'else' after expression to 'then'");
        ++it;

        // if COND then EXPR else <EXPR>
        auto else_ = expression(it, end);
        if (!else_)
            throw std::runtime_error("expected expression after 'else'");
        it = else_->to();

        return shared_ptr<AST::IfThenElse>(new AST::IfThenElse(start, it, cond, then_, else_));
    }



    shared_ptr<AST::Terminal> terminal (token_iter it, token_iter end)
    {
        if (it==end)
            return shared_ptr<AST::Terminal>();
        if (auto e = if_then_else(it, end))
            return e;
        if (auto e = binding(it, end))
            return shared_ptr<AST::Terminal>(); // just catch this here
        if (auto e = call(it, end))
            return e;
        if (auto e = identifier(it, end))
            return e;
        if (auto e = integer_literal(it, end))
            return e;
        if (auto e = real_literal(it, end))
            return e;
        if (auto e = bool_literal(it, end))
            return e;
        if (auto e = unary(it, end))
            return e;
        if (it->kind == LParen) {
            if (auto e = expression(it+1, end)) {
                if (e->to()->kind != RParen)
                    throw std::runtime_error("missing ')', got '" + string(*e->to()) + "'");
                return shared_ptr<AST::ParenExpression>(
                            new AST::ParenExpression(it, e->to()+1, e));
            }
        }
        if (it->kind == Let) {
            const auto start = it;
            vector<shared_ptr<AST::Binding>> bindings;

            ++it;
            while (it != end) {
                auto e = binding(it, end);
                if (!e)
                    throw std::runtime_error("only bindings allowed within 'let/in'-sequence");
                it = e->to();
                bindings.push_back(e);

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
            auto value = expression(it, end);
            if (!value)
                throw std::runtime_error("missing value expression after 'in'");
            it = value->to();

            return shared_ptr<AST::LetIn>(new AST::LetIn(start, it, bindings, value));
        }
        return shared_ptr<AST::Terminal>();
    }



    shared_ptr<AST::Expression> binary(int min_prec, shared_ptr<AST::Expression> lhs,
                                       token_iter it, token_iter end)
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
            shared_ptr<AST::Expression> rhs = terminal(it, end);
            if (!rhs)
                throw std::runtime_error("expected operand on right-hand-side of operator "
                                         "'" + prec_entry->second.symbol + "'");
            it = rhs->to();

            // 1 + 2 ? 3
            //       ^
            if (it != end) {
                const int next_prec = precedence(string(*it));
                if (prec < next_prec) {
                    // This means '?' has higher precedence (e.g. '*').
                    rhs = binary(next_prec, rhs, it, end); // Therefore, what is our rhs should really be ?'s lhs.
                    if (!rhs) {
                        return shared_ptr<AST::Expression>();
                    }
                    it = rhs->to();
                }
            }
            lhs = prec_entry->second.create(lhs->from(), rhs->to(), lhs, rhs);
        }
    }



    shared_ptr<AST::Expression> expression(token_iter it, token_iter end)
    {
        using namespace AST;
        if (it == end)
            return shared_ptr<AST::Expression>();
        auto lhs = terminal(it, end);
        if (!lhs) {
            return shared_ptr<Expression>();
        }
        it = lhs->to();
        return binary(0, lhs, it, end);
    }

} } } }

namespace excyrender { namespace Nature { namespace Et1 { namespace AST {

    shared_ptr<AST::Program> program(token_iter it, token_iter end) {
        // TODO: extract the following as "bindings(it,end, Static, Dynamic)", same for "LetIn"
        if (it == end) {
            return shared_ptr<AST::Program>();
        }
        if (it->kind == Static) {
            const auto start = it;
            vector<shared_ptr<AST::Binding>> bindings;

            ++it;
            while (it != end) {
                auto e = binding(it, end);
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
            auto value = expression(it, end);
            if (!value)
                throw std::runtime_error("missing value expression after 'dynamic'");
            it = value->to();

            return shared_ptr<AST::Program>(new AST::Program(start, it, bindings, value));
        }
        // Single top level binding.
        else if (auto e = binding(it, end)) {
            if (e->to() != end)
                throw std::runtime_error("stray input after top level binding");
            if (e->is_generic())
                throw std::runtime_error("top level binding must not be generic");
            return shared_ptr<AST::Program>(
                      new AST::Program(it, end,
                                       vector<shared_ptr<AST::Binding>>(),
                                       e));
        }
        // Single top level expression.
        else if (auto e = expression(it, end)) {
            if (e->to() != end)
                throw std::runtime_error("stray input after top level binding");
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

        shared_ptr<AST::Program> prog = program(toks.begin(), toks.end());
        if (!prog)
            throw std::runtime_error("not compilable: " +
                                     string(toks.begin()->from, (toks.end()-1)->to) +
                                     "(" + __func__ + ")");

        ASTPasses::lambda_lift(prog);

        throw std::runtime_error("'" + string(__func__) + "' not fully implemented");
    }

} } } }



// -- API ------------------------------------------------------------------------------------------
namespace excyrender { namespace Nature { namespace Et1 {

HeightFunction compile (std::string const &code) {
    return AST::compile(tokenize(code));
}

} } }
