// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "1100_resolve_types.hh"
#include "../ASTQueries/resolve_type.hh"

#include <stack>
#include <iostream>
#include <stdexcept>
#include <algorithm>



//- Tests ------------------------------------------------------------------------------------------
#include "../UnitTesting.hh"
#include "../ASTPrinters/PrettyPrinter.hh"

TEST_CASE( "Et1/ASTPasses/1100_resolve_types.hh", "Type resolution" ) {

    using namespace excyrender::Nature::Et1;
    using detail::equal;

    auto passes = [](std::shared_ptr<AST::Program> ast) { ASTPasses::resolve_types(ast); };

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

    REQUIRE(equal("let f(x) = x in f(2)",

                  "let f(x) = x, "
                  "    int f(int x) = x "
                  "in f(2)",
                  passes));

    REQUIRE(equal("let f(x) = x in f(2.0)",

                  "let f(x) = x, "
                  "    float f(float x) = x "
                  "in f(2.0)",
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
}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

namespace {
    using namespace AST;
    using std::set;
    using std::stack;
    using std::string;

    struct ResolveTypes final : Transform {
        bool transformed() const { return transformed_; }
        bool has_unresolved() const { return has_unresolved_; }

        void begin(Addition &) {}
        void end(Addition &bin) { resolve(bin); }

        void begin(Subtraction &) {}
        void end(Subtraction &bin) { resolve(bin); }

        void begin(Multiplication &) {}
        void end(Multiplication &bin) { resolve(bin); }

        void begin(Division &) {}
        void end(Division &bin) { resolve(bin); }

        void transform(IntegerLiteral &term) { resolve(term); }
        void transform(RealLiteral &term) { resolve(term); }
        void transform(AST::Identifier &id) { resolve(id); }

        void begin(Call &) {}


        static Binding* lookup (Call &call, vector<Binding*> visible_bindings)
        {
            auto fitness = [&](Binding &b) -> int {
                if (b.id() != call.id())
                    return -1;
                if (b.arguments().size() != call.arguments().size())
                    return -1;
                // count the number of matchin arguments.
                int f = 0;
                for (size_t i=0; i<b.arguments().size(); ++i) {
                    if (b.arguments()[i].type == "auto") {
                        f += 1;
                    } else if (b.arguments()[i].type == call.arguments()[i]->type()) {
                        f += 1+b.arguments().size();
                    } else {
                        return -1;
                    }
                }
                return f;
            };

            Binding* binding = nullptr;
            int best_fitness = 0;
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

        void end(Call &call)
        {
            for (auto &arg : call.arguments()) {
                string type = arg->type();
                if (type.empty() || type[0] == '<' || type=="auto") {
                    has_unresolved_ = true;
                    return;
                }
            }

            Binding* binding = lookup(call, scope.top().visible_bindings);
            if (binding) {
                if (call.type() == "auto" && binding->type()!="auto" && binding->type()[0]!='<') {
                    call.reset_type(binding->type()); // TODO: execute this every time, but need to remember bound binding
                    transformed_ = true;
                }
                // TODO: remember the binding in the call
                bool is_generic = false;
                for (auto arg : binding->arguments()) {
                    if (arg.type == "auto") {
                        is_generic = true;
                        break;
                    }
                }
                if (is_generic) {
                    if (Binding *insta = scope.top().instantiate(*binding, call.arguments())) {
                        insta->accept(*this);
                        if (call.type() == "auto" && binding->type()!="auto" && binding->type()[0]!='<') {
                            call.reset_type(insta->type());
                            transformed_ = true;
                        }
                    }
                }
            }
        }

        void begin(Negation &) {}
        void end(Negation &neg) { resolve(neg); }

        void begin(ParenExpression &) {}
        void end(ParenExpression &p) { resolve(p); }

        void begin(Binding &binding)
        {
            scope.push(scope.top().enter_binding(binding));
        }
        void end(Binding &binding)
        {
            resolve(binding.body(), binding);
            scope.pop();
        }

        void begin(LetIn &letin) {
            scope.push(scope.top().enter_declarative_region(letin.bindings()));
        }
        void end(LetIn &letin) {
            resolve(letin.value(), letin);
            scope.pop();
        }

        void begin(Program &p) {
            scope.push(Scope::EnterProgram(p.bindings()));
        }
        void end(Program &p) {
            resolve(p.value(), p);
            scope.pop();
        }

    private:
         bool transformed_ = false;
         bool has_unresolved_ = false;


         void resolve (ASTNode &binary) {
             auto type = ASTQueries::resolve_type(binary, scope.top().symbols);
             auto btype = binary.type();
             if (binary.type() == "auto") {
                 if (type[0] == '<' || type=="auto") {
                     has_unresolved_ = true;
                 } else {
                     binary.reset_type(type);
                     transformed_ = true;
                 }
             } else if (binary.type() != type) {
                 throw std::logic_error("type clash in resolve_binary");
             }
         }

         void resolve (ASTNode &ast, ASTNode &update_to) {
             resolve(ast);

             string value_type = ast.type();
             if (value_type != "auto" && value_type[0] != '<') {
                 if (update_to.type() == "auto") {
                     update_to.reset_type(value_type);
                     transformed_ = true;
                 } else if (value_type != update_to.type()) {
                     throw std::runtime_error("declared function type does not equal body type");
                 }
             } else if (value_type != "auto") {
                 has_unresolved_ = true;
             }
         }

         struct Scope {
             std::map<string, string> symbols;
             vector<shared_ptr<Binding>> *bindings_declarative_region;
             vector<Binding*> visible_bindings;

             Scope enter_binding(Binding &binding) const {
                Scope ret;
                ret.visible_bindings = visible_bindings;
                ret.visible_bindings.push_back(&binding);
                ret.bindings_declarative_region = bindings_declarative_region;

                for (auto a : binding.arguments()) {
                    ret.symbols[a.name] = a.type;
                }
                return ret;
             }

             Scope enter_declarative_region(vector<shared_ptr<Binding>> &bindings_declarative_region) const {
                Scope ret;
                ret = *this;
                for (auto b : bindings_declarative_region)
                    ret.visible_bindings.push_back(&*b);
                ret.bindings_declarative_region = &bindings_declarative_region;
                return ret;
             }

             static Scope EnterProgram (vector<shared_ptr<Binding>> &bindings_declarative_region) {
                Scope ret;
                for (auto b : bindings_declarative_region)
                    ret.visible_bindings.push_back(&*b);
                ret.bindings_declarative_region = &bindings_declarative_region;
                return ret;
             }

             // Returns: new     <- instantiation happened.
             //          nullptr <- nothing happened.
             Binding* instantiate(Binding& binding, vector<shared_ptr<Expression>> const &args) {
                if (args.size() != binding.arguments().size())
                    throw std::runtime_error("wrong number of arguments in call to " + binding.id());

                // Check if this is the optimal candidate already.
                // If so, do not re-instantiate.
                bool is_optimal = true;
                for (size_t a=0, num_args=args.size(); a!=num_args; ++a) {
                    if (binding.arguments()[a].type != args[a]->type()) {
                        is_optimal = false;
                        break;
                    }
                }
                if (is_optimal)
                    return nullptr;

                shared_ptr<Binding> insta (binding.deep_copy());

                for (size_t a=0, num_args=insta->arguments().size(); a!=num_args; ++a) {
                    const auto &desired = args[a]->type();
                    Argument &arg = insta->arguments()[a];

                    if (arg.type != "auto" && desired != arg.type)
                        throw std::runtime_error("cannot instantiate");
                    arg.type = desired;
                }

                bindings_declarative_region->push_back(insta);
                visible_bindings.push_back(&*insta);

                return &*insta;
             }
         };

         std::stack<Scope> scope;
    };
}

void resolve_types(shared_ptr<AST::ASTNode> ast) {
    if (!ast) {
        //std::clog << "pass: lambda-lifting skipped, AST is empty\n";
        return;
    }

    int i = 0;
    while (1) {
        ++i;
        ResolveTypes ll;
        ast->accept(ll);
        if (!ll.transformed()) {
            /*if (ll.has_unresolved()) {
                throw std::runtime_error("program is not fully resolvable");
            }*/
            break;
        }

        /*ASTPrinters::PrettyPrinter pp(std::cerr);
        ast->accept(pp);
        std::cerr << "\n";*/
    }
    std::clog << "pass: resolve-types (" << i << "x)\n";
    // TODO: we need another pass for resolving, were we cancel out unused functions,
    //       because it might be that there are still unresoved bindings, which are only
    //       unresolved because they are generic.
}


} } } }
