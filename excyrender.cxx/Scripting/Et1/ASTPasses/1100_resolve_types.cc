// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "1100_resolve_types.hh"
#include "../ASTQueries/resolve_type.hh"
#include "../ASTQueries/has_unresolved.hh"
#include "../ASTQueries/equal.hh"

#include <stack>
#include <iostream>
#include <stdexcept>
#include <algorithm>



//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../Backends/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTPasses/1100_resolve_types.hh", "Type resolution" ) {

    using namespace excyrender::Nature::Et1;
    using detail::equal;

    auto passes = [](std::shared_ptr<AST::Program> ast) { ASTPasses::resolve_types(ast); };

    REQUIRE(equal("let y = true in y",
                  "let bool y = true in y",
                  passes));

    // TODO: check exception
    /*REQUIRE(equal("if 1 then true else 1",
                  "if 1 then true else 1",
                  passes));*/

    REQUIRE(equal("let y = if true then true else false || true in y",
                  "let bool y = if true then true else false || true in y",
                  passes));

    REQUIRE(equal("let y = 1 in y",
                  "let int y = 1 in y",
                  passes));

    REQUIRE(equal("let auto y = 1 in y",
                  "let int y = 1 in y",
                  passes));

    REQUIRE(equal("let f(int x) = 1 in f(2)",
                  "let int f(int x) = 1 in f(2)",
                  passes));

    REQUIRE(equal("let f(int x) = x in f(2)",
                  "let int f(int x) = x in f(2)",
                  passes));

    REQUIRE(equal("let q(x) = x in q(2)",

                  "let q(x) = x, "
                  "    int q(int x) = x "
                  "in q(2)",
                  passes));

    REQUIRE(equal("let q(x) = x in q(2.0)",

                  "let q(x) = x, "
                  "    float q(float x) = x "
                  "in q(2.0)",
                  passes));

    REQUIRE(equal("let q(x) = x in q(true)",

                  "let q(x) = x, "
                  "    bool q(bool x) = x "
                  "in q(true)",
                  passes));


    REQUIRE(equal("let f(x) = 1 in f(2.0)",

                  "let int f(x) = 1, "
                  "    int f(float x) = 1 "
                  "in f(2.0)",
                  passes));

    REQUIRE(equal("let f(x) = x, "
                  "    y = f(2.0) "
                  "in f(2)",

                  "let f(x) = x, "
                  "    float y = f(2.0), "
                  "    float f(float x) = x, "
                  "    int f(int x) = x "
                  "in f(2)",
                  passes));


    REQUIRE(equal("let f(x) = 1.0 in f(2)",
                  "let float f(x) = 1.0, float f(int x) = 1.0 in f(2)",
                  passes));

    REQUIRE(equal("let f(x) = 1, "
                  "    a = f(1), "
                  "    b = f(1.0) "
                  "in 1",

                  "let int f(x) = 1, "
                  "    int a = f(1), "
                  "    int b = f(1.0), "
                  "    int f(int x) = 1, "
                  "    int f(float x) = 1 "
                  "in 1",
                  passes));

    REQUIRE(equal("let f(x,y) = x, "
                  "    a = f(1, 1),  "
                  "    b = f(1.0, 1), "
                  "    c = f(1, 1.0), "
                  "    d = f(1.0, 1.0) "
                  "in 1",

                  "let f(x,y) = x, "
                  "    int   a = f(1, 1),  "
                  "    float b = f(1.0, 1), "
                  "    int   c = f(1, 1.0), "
                  "    float d = f(1.0, 1.0), "

                  "    int   f(int x, int y) = x, "
                  "    float f(float x, int y) = x, "
                  "    int   f(int x, float y) = x, "
                  "    float f(float x, float y) = x "
                  "in 1",
                  passes));

    REQUIRE(equal("let f(x) = x,    "
                  "    g(x) = f(x), "
                  "    h(x) = g(x)  "
                  " in h(1.0)       ",

                  "let f(x) = x,     "
                  "    g(x) = f(x),  "
                  "    h(x) = g(x),  "
                  "    float h(float x) = g(x), "
                  "    float g(float x) = f(x), "
                  "    float f(float x) = x "
                  " in h(1.0)       ",
                  passes));

    REQUIRE(equal("let f(x) = "
                  "   let g(x) = "
                  "      let h(x) = x "
                  "      in h(x) "
                  "   in g(x) "
                  "in f(2.0) ",

                  "let f(x) = "
                  "   let g(x) = "
                  "      let h(x) = x "
                  "      in h(x) "
                  "   in g(x), "
                  "  float f(float x) = "
                  "   let g(x) = "
                  "        let h(x) = x "
                  "        in h(x), "
                  "    float g(float x) = "
                  "        let h(x) = x, "
                  "            float h(float x) = x "
                  "        in h(x) "
                  "   in g(x) "
                  "in f(2.0) ",
                  passes));

    return; // TODO: include below test in later function. it's a test for lambda-lifting and type-res.
    REQUIRE(equal("let y=2, f(x) = y in f(y)",
                  "let int y=2, f(x,y) = y, int f(int x, int y) = y in f(y,y)",
                  passes));

}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

namespace {
    using namespace AST;
    using std::set;
    using std::stack;
    using std::string;

    struct ResolveTypes final : Transform {

        void begin(AST::Addition &) {}
        void end(AST::Addition &bin) { resolve(bin); }
        void begin(AST::Subtraction &) {}
        void end(AST::Subtraction &bin) { resolve(bin); }
        void begin(AST::Multiplication &) {}
        void end(AST::Multiplication &bin) { resolve(bin); }
        void begin(AST::Division &) {}
        void end(AST::Division &bin) { resolve(bin); }

        void begin(AST::LessThan &) {}
        void end(AST::LessThan &bin) { resolve(bin); }
        void begin(AST::LessEqual &) {}
        void end(AST::LessEqual &bin) { resolve(bin); }
        void begin(AST::GreaterThan &) {}
        void end(AST::GreaterThan &bin) { resolve(bin); }
        void begin(AST::GreaterEqual &) {}
        void end(AST::GreaterEqual &bin) { resolve(bin); }
        void begin(AST::Equal &) {}
        void end(AST::Equal &bin) { resolve(bin); }
        void begin(AST::NotEqual &) {}
        void end(AST::NotEqual &bin) { resolve(bin); }
        void begin(AST::LogicalAnd &) {}
        void end(AST::LogicalAnd &bin) { resolve(bin); }
        void begin(AST::LogicalOr &) {}
        void end(AST::LogicalOr &bin) { resolve(bin); }
        void begin(AST::LogicalNot &) {}
        void end(AST::LogicalNot &u) { resolve(u); }

        void transform(IntegerLiteral &term) { resolve(term); }
        void transform(RealLiteral &term) { resolve(term); }
        void transform(BoolLiteral &term) { resolve(term); }

        void transform(AST::Identifier &id) {
            resolve(id);
            if (!id.type()) {
                // Try to look up bindings, which are not in the symbol table.
                Call call(id.from(), id.to(), id.id(), {});
                begin(call);
                end(call);
                if (call.type())
                    id.reset_type(call.type());
            }
        }

        void begin(AST::Call &) {}


        static shared_ptr<Binding> lookup (Call const &call, vector<shared_ptr<Binding>> visible_bindings)
        {
            auto fitness = [&](Binding &b) -> int {
                if (b.id() != call.id())
                    return -2;
                if (b.arguments().size() != call.arguments().size())
                    return -2;
                // count the number of matching arguments.
                int f = 0;
                for (size_t i=0; i<b.arguments().size(); ++i) {
                    if (!b.arguments()[i].type) {
                        f += 1;
                    } else if (b.arguments()[i].type == call.arguments()[i]->type()) {
                        f += 1+b.arguments().size();
                    } else {
                        return -1;
                    }
                }
                return f;
            };

            shared_ptr<Binding> binding;
            int best_fitness = -1;
            bool ambiguous = false;

            for (auto b : visible_bindings) {
                int f = fitness(*b);

                if (f > best_fitness) {
                    binding = b; // TODO: build list of candidates
                    best_fitness = f;
                    ambiguous = false;
                } else if (f == best_fitness) {
                    ambiguous = true;
                }
            }

            if (ambiguous) {
                throw std::runtime_error("multiple instantiations of " + call.id() + " are ambiguous");
            }

            return binding;
        }

        void begin(AST::IfThenElse &) {}
        void end(AST::IfThenElse &t) { resolve(t); }

        void end(AST::Call &call)
        {
            for (auto &arg : call.arguments()) {
                if (!arg->type()) {
                    return;
                }
            }

            shared_ptr<Binding> binding = lookup(call, scope.top().visible_bindings);
            if (binding) {
                // If the looked up function is generic, we need to instantiate a fitting version.
                if (!binding->is_generic()) {
                    // NON-GENERIC
                    if (!call.type() && binding->type()) {
                        call.reset_type(binding->type()); // TODO: execute this every time, but need to remember bound binding
                        call.reset_referee(binding);
                    }
                } else {
                    // GENERIC
                    if (auto insta = scope.top().instantiate(*binding, call.arguments())) {
                        insta->accept(*this);
                        if (!call.type() && insta->type()) {
                            call.reset_type(insta->type());
                            call.reset_referee(insta);
                        } else if (call.type() != binding->type()) {
                            throw std::logic_error ("impossible (2)");
                        }
                    } else {
                        // this would indicate a bug in lookup
                        throw std::logic_error("impossible");
                    }
                }
            } else {
                //throw std::runtime_error("unresolved call to '" + call.id() + "'");
            }
        }

        void begin(AST::Negation &) {}
        void end(AST::Negation &neg) { resolve(neg); }

        void begin(AST::ParenExpression &) {}
        void end(AST::ParenExpression &p) { resolve(p); }

        void begin(AST::Binding &binding_)
        {
            shared_ptr<Binding> binding;
            for (auto b : *scope.top().bindings_declarative_region) {
                if (b.get() == &binding_) {
                    binding = b;
                    break;
                }
            }
            if (!binding) {
                throw std::logic_error("internal: couldn't find shared_ptr corresponding to binding");
            }
            scope.push(scope.top().enter_binding(binding));
        }
        void end(AST::Binding &binding)
        {
            resolve(binding.body(), binding);
            scope.pop();
        }

        void begin(AST::LetIn &letin) {
            scope.push(scope.top().enter_declarative_region(letin.bindings()));
        }
        void end(AST::LetIn &letin) {
            resolve(letin.value(), letin);
            scope.pop();
        }

        void begin(AST::Program &p) {
            scope.push(Scope::EnterProgram(p.bindings()));
        }
        void end(AST::Program &p) {
            resolve(p.value(), p);
            scope.pop();
        }

    private:


         void update_into(ASTNode &ast, Typeinfo type, std::string const &clash_err_msg){
             if (!ast.type()) {
                 if (type) {
                     ast.reset_type(type);
                 }
             } else if (type && ast.type() != type) {
                 throw std::runtime_error(clash_err_msg);
             }
         }

         void resolve (ASTNode &ast) {
             auto type = ASTQueries::resolve_type(ast, scope.top().symbols);
             update_into(ast, type, "type clash in resolve(AST&)");
         }

         void resolve (ASTNode &ast, ASTNode &update_to) {
             resolve(ast);
             update_into(update_to, ast.type(), "declared function type does not equal body type");
         }

         struct Scope {
             std::map<string, Typeinfo> symbols;
             vector<shared_ptr<Binding>> *bindings_declarative_region;
             vector<shared_ptr<Binding>> visible_bindings;

             Scope enter_binding(shared_ptr<Binding> binding) const {
                Scope ret;
                ret.visible_bindings = visible_bindings;
                ret.visible_bindings.push_back(binding);
                ret.bindings_declarative_region = bindings_declarative_region;

                for (auto a : binding->arguments()) {
                    ret.symbols[a.name] = a.type;
                }
                return ret;
             }

             Scope enter_declarative_region(vector<shared_ptr<Binding>> &bindings_declarative_region) const {
                Scope ret;
                ret = *this;
                for (auto b : bindings_declarative_region)
                    ret.visible_bindings.push_back(b);
                ret.bindings_declarative_region = &bindings_declarative_region;
                return ret;
             }

             static Scope EnterProgram (vector<shared_ptr<Binding>> &bindings_declarative_region) {
                Scope ret;
                for (auto b : bindings_declarative_region)
                    ret.visible_bindings.push_back(b);
                ret.bindings_declarative_region = &bindings_declarative_region;
                return ret;
             }

             // Returns: new     <- instantiation happened.
             //          nullptr <- nothing happened.
             shared_ptr<Binding> instantiate(Binding const& binding, vector<shared_ptr<Expression>> const &args) {
                if (args.size() != binding.arguments().size())
                    throw std::runtime_error("wrong number of arguments in call to " + binding.id());

                // TODO: check if all arguments were resolved. Return nullptr if not.

                // Check if this is the optimal candidate already.
                // If so, do not re-instantiate.
                bool is_optimal = true;
                for (size_t a=0, num_args=args.size(); a!=num_args; ++a) {
                    if (binding.arguments()[a].type != args[a]->type()) {
                        is_optimal = false;
                        break;
                    }
                }

                if (is_optimal) {
                    return nullptr;
                }

                shared_ptr<Binding> insta (binding.deep_copy());

                for (size_t a=0, num_args=insta->arguments().size(); a!=num_args; ++a) {
                    const auto &desired = args[a]->type();
                    Argument &arg = insta->arguments()[a];

                    if (arg.type && desired != arg.type)
                        throw std::runtime_error("cannot instantiate");
                    arg.type = desired;
                }

                bindings_declarative_region->push_back(insta);
                visible_bindings.push_back(insta);

                return bindings_declarative_region->back();
             }
         };

         std::stack<Scope> scope;
    };
}


void resolve_types(shared_ptr<AST::ASTNode> ast) {
    if (!ast) {
        return;
    }

    decltype(ast) prev_ast;
    int i = 0;
    while (1) {
        ++i;

        ResolveTypes ll;
        prev_ast.reset (ast->deep_copy());
        ast->accept(ll);
        if (!ASTQueries::has_unresolved(ast)) {
            break;
        } else if (ASTQueries::equal(*prev_ast, *ast)) {
            throw std::runtime_error("program not fully resolvable");
        }

        if (i>4096) {
            ASTPrinters::PrettyPrinter pp(std::cerr);
            ast->accept(pp);
            std::cerr << "\n";
            throw std::logic_error("Et1::resolve_types is running for a long while now, "
                                   "probably it is stuck. May I please beg for a few "
                                   "seconds of your time and kindly ask you to mail all "
                                   "generated output to \"mach.seb@gmail.com\". Thank you.");

        }
    }
    //std::clog << "pass: resolve-types (" << i << "x)\n";
}


} } } }
