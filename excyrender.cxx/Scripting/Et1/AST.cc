// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "AST.hh"
#include <map>
#include <stdexcept>
#include <iostream>

namespace excyrender { namespace Nature { namespace Et1 { namespace {


    struct DumpVisitor final : AST::Visitor {

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

    private:
        std::ostream &os = std::cout;
        int indent_ = 0;
        void indent() {
            for (int i=0; i!=indent_; ++i) {
                std::cout << "    ";
            }
        }
    };


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


    shared_ptr<AST::Expression>     expression(token_iter, token_iter);
    shared_ptr<AST::IntegerLiteral> integer_literal(token_iter, token_iter);
    shared_ptr<AST::Terminal>       terminal (token_iter it, token_iter end);


    // function-call : identifier '(' expression (',' expression)* ')'
    shared_ptr<AST::Call> call(token_iter it, token_iter end)
    {
        using namespace AST;

        const token_iter call_begin = it;

        // some-call ( foo , bar )
        // ^^^^^^^^^
        if (it->kind != Identifier)
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

        return shared_ptr<Call>(new Call(call_begin, it, callee, arguments));
    }



    shared_ptr<AST::IntegerLiteral> integer_literal(token_iter it, token_iter end)
    {
        using namespace AST;

        if (it->kind != Integer) return shared_ptr<IntegerLiteral>();
        return shared_ptr<IntegerLiteral>(new IntegerLiteral(it, it+1));
    }



    shared_ptr<AST::Unary> unary(token_iter it, token_iter end)
    {
        using namespace AST;

        if (it->kind == Minus) {
            auto rhs = terminal(it+1, end);
            if (!rhs)
                throw std::runtime_error("expected argument to negation-operator");
            return shared_ptr<Negation>(new Negation(it, rhs->to(), rhs));
        }

        return shared_ptr<AST::Unary>();
    }



    shared_ptr<AST::Terminal> terminal (token_iter it, token_iter end)
    {
        if (auto e = integer_literal(it, end))
            return e;
        if (auto e = unary(it, end))
            return e;
        if (auto e = call(it, end))
            return e;
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
                throw std::runtime_error("expected operand on right-hand-side of operator");
            it = rhs->to();

            // 1 + 2 ? 3
            //       ^
            const int next_prec = precedence(string(*it));
            if (prec < next_prec) {
                // This means '?' has higher precedence (e.g. '*').
                rhs = binary(next_prec, rhs, it, end); // Therefore, what is our rhs should really be ?'s lhs.
                if (!rhs)
                    return shared_ptr<AST::Expression>();
                it = rhs->to();
            }

            lhs = prec_entry->second.create(lhs->from(), rhs->to(), lhs, rhs);
        }
    }



    shared_ptr<AST::Expression> expression(token_iter it, token_iter end)
    {
        using namespace AST;

        auto lhs = terminal(it, end);
        if (!lhs) return shared_ptr<Expression>();
        it = lhs->to();
        return binary(0, lhs, it, end);
    }


    HeightFunction compile (vector<Token> const &toks) {
        if (toks.empty())
            throw std::runtime_error("no tokens");
        std::cout << toks << std::endl;
        if (auto e = expression(toks.begin(), toks.end())) {
            std::cout << "expression found" << std::endl;

            DumpVisitor dumper;
            e->accept(dumper);
        }
        throw std::runtime_error("not implemented");
    }

} } } }



// -- API ------------------------------------------------------------------------------------------
namespace excyrender { namespace Nature { namespace Et1 {

HeightFunction compile (std::string const &code) {
    return compile(tokenize("1+2*3+4"));
}

} } }
