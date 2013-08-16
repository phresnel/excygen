// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Et1.hh"
#include "optional.hh"
#include "memory.hh"
#include <stdexcept>
#include <vector>
#include <algorithm>
#include <map>


// -- Tokenization ---------------------------------------------------------------------------------
namespace excyrender { namespace Nature { namespace Et1 { namespace {

    using string = std::string;
    using iterator = string::const_iterator;
    using std::vector;

    enum TokenKind {
        Integer,
        Identifier,
        LParen, RParen,
        Plus, Minus,
        Asterisk, Slash,
        Comma
    };

    struct Token {
        TokenKind kind;
        iterator from, to;

        bool operator== (const char *c_str) const {
            return string(from,to) == c_str;
        }

        bool operator!= (const char *c_str) const {
            return !(*this == c_str);
        }

        operator string() const {
            return string(from,to);
        }

        Token (TokenKind kind, iterator from, iterator to) : kind(kind), from(from), to(to) {}
    };

    using token_iter = vector<Token>::const_iterator;


    std::ostream& operator<< (std::ostream &os, Token const &tok) {
        switch (tok.kind) {
        case Integer: os << "[int:"; break;
        case LParen: os << "'('"; break;
        case RParen: os << "')'"; break;
        case Plus: os << "'+'"; break;
        case Minus: os << "'-'"; break;
        case Asterisk: os << "'*'"; break;
        case Slash: os << "'/'"; break;
        case Comma: os << "','"; break;
        case Identifier: os << "[id:"; break;
        }
        return os << string(tok.from, tok.to) << "] ";
    }

    std::ostream& operator<< (std::ostream &os, vector<Token> const &toks) {
        for (auto tok : toks) {
            os << tok;
        }
        return os;
    }



    // -- Tokenizer Helpers ------------------------------------------------------------------------
    namespace detail {

        bool is_digit(char c) {
            return c>='0' && c<='9'; // C++ guarantees that 0..9 are contiguous.
        }

        bool is_uppercase_letter(char c) {
            switch (c) {
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
            case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
            case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
            case 'V': case 'W': case 'X': case 'Y': case 'Z':
                return true;
            }
            return false;
        }

        bool is_lowercase_letter(char c) {
            switch (c) {
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
            case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
            case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
            case 'v': case 'w': case 'x': case 'y': case 'z':
                return true;
            }
            return false;
        }

        bool is_letter(char c) {
            return is_uppercase_letter(c) || is_lowercase_letter(c);
        }

        optional<iterator> integer(iterator it, iterator end) {
            int sign = 1;
            while (it != end) {
                if (*it == '-') {
                    sign = -sign;
                    ++it;
                } else if (*it == '+') {
                    ++it;
                } else {
                    break;
                }
            }
            if (it==end || !is_digit(*it))
                return optional<iterator>();

            while (is_digit(*it)) {
                ++it;
            }
            return it;
        }

        optional<iterator> identifier(iterator it, iterator end) {
            if (!is_letter(*it))
                return optional<iterator>();
            ++it;
            while (it!=end && (is_letter(*it) || is_digit(*it)))
                ++it;
            return it;
        }
    }


    // -- Tokenizer --------------------------------------------------------------------------------
    std::vector<Token> tokenize(std::string const &str)
    {
        using namespace detail;
        std::vector<Token> tokens;

        std::cout << "tokenizing [" << str << "]\n";
        for (auto it = str.begin(), end=str.end(); it!=end; ) {
            if (auto oit = integer(it, end)) {
                tokens.emplace_back(Integer, it, *oit);
                it = *oit;
            } /* else if (auto oit = operator_(it, end)) {
                tokens.emplace_back(Operator, it, *oit);
                it = *oit;
            }*/
            else if (*it == '(') {
                tokens.emplace_back(LParen, it, it+1);
                ++it;
            } else if (*it == ')') {
                tokens.emplace_back(RParen, it, it+1);
                ++it;
            } else if (*it == '+') {
                tokens.emplace_back(Plus, it, it+1);
                ++it;
            } else if (*it == '-') {
                tokens.emplace_back(Minus, it, it+1);
                ++it;
            } else if (*it == '*') {
                tokens.emplace_back(Asterisk, it, it+1);
                ++it;
            } else if (*it == '/') {
                tokens.emplace_back(Slash, it, it+1);
                ++it;
            } else if (*it == ',') {
                tokens.emplace_back(Comma, it, it+1);
                ++it;
            } else if (auto oit = identifier(it, end)) {
                tokens.emplace_back(Identifier, it, *oit);
                it = *oit;
            } else {
                throw std::runtime_error("tokenization failure: ..." + string(it, end));
            }
        }
        return tokens;
    }

} } } }



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



namespace excyrender { namespace Nature { namespace Et1 { namespace {

    optional<Token> lookahead(token_iter it, token_iter end)  {
        if (it != end && it+1 != end)
            return *it;
        return optional<Token>();
    }

    bool lookahead_compare(token_iter it, token_iter end, TokenKind kind)  {
        if (auto x = lookahead(it, end))
            return x->kind == kind;
        return false;
    }



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


    shared_ptr<AST::Expression> expression(token_iter, token_iter);
    shared_ptr<AST::IntegerLiteral> integer_literal(token_iter, token_iter);


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



    shared_ptr<AST::Terminal> terminal (token_iter it, token_iter end)
    {
        if (auto e = integer_literal(it, end))
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

            std::map<string, BinaryOperator>::iterator prec_entry;
            const int prec = precedence(string(*it), &prec_entry);
            if (prec < min_prec)
                return lhs;

            ++it;
            shared_ptr<AST::Expression> rhs = terminal(it, end);
            if (!rhs)
                throw std::runtime_error("expected operand on right-hand-side of operator");
            ++it;

            const int next_prec = precedence(string(*it));
            if (prec < next_prec) {
                rhs = binary(prec+1, rhs, it+1, end);
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



    // factor
    //     : function-call
    //     | scalar
    //     | '(' expression ')'
    /*token_iter factor(token_iter it, token_iter end) {
        if (function_call
        //return it;
    }*/

    HeightFunction compile (vector<Token> const &toks) {
        if (toks.empty())
            throw std::runtime_error("no tokens");
        if (expression(toks.begin(), toks.end())) {
            std::cout << "expression found" << std::endl;
        }
        throw std::runtime_error("not implemented");
    }

} } } }



// -- API ------------------------------------------------------------------------------------------
namespace excyrender { namespace Nature { namespace Et1 {

HeightFunction compile (std::string const &code) {
    return compile(tokenize(code));
}

} } }
