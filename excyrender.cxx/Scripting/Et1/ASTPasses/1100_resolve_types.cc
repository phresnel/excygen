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

    /*REQUIRE(equal("let f(x) = y in f(2)",  // TODO: make this fail
                  "let int f(int x) = 1 in f(2)",
                  passes));*/

    REQUIRE(equal("let f(int x) = 1 in f(2)",
                  "let int f(int x) = 1 in f(2)",
                  passes));

    REQUIRE(equal("let f(int x) = x in f(2)",
                  "let int f(int x) = x in f(2)",
                  passes));

    REQUIRE(equal("let f(x) = x in f(2)",

                  "let int f(x) = x, "
                  "    int f(int x) = x "
                  "in f(2)",
                  passes));

return;

    REQUIRE(equal("let f(x) = 1.0 in f(2)",
                  "let float f(int x) = 1.0 in f(2)",
                  passes));

    REQUIRE(equal("let f(x) = 1.0 in f(2)",
                  "let float f(float x) = 1.0 in f(2.0)",
                  passes));

    // This tests auto-emitted overloads.
    REQUIRE(equal("let f(x) = 1, "
                  "    a = f(1), "
                  "    b = f(1.0) "
                  "in 1",

                  "let int f(int x) = 1, "
                  "    int f(float x) = 1, "
                  "    int a = f(1), "
                  "    int b = f(1.0) "
                  "in 1",
                  passes));

    REQUIRE(equal("let f(x,y) = 1, "
                  "    a = f(1, 1),  "
                  "    b = f(1.0, 1), "
                  "    c = f(1, 1.0), "
                  "    d = f(1.0, 1.0) "
                  "in 1",

                  "let int   f(int x, int y) = x, "
                  "    float f(float x, int y) = x, "
                  "    int   f(int x, float y) = x, "
                  "    float f(float x, float y) = x, "
                  "    int   a = f(1, 1),  "
                  "    float b = f(1.0, 1), "
                  "    int   c = f(1, 1.0), "
                  "    float d = f(1.0, 1.0) "
                  "in 1",
                  passes));

    // TODO: check for thrown exceptions upon type mismatches
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
        void end(Addition &) {}

        void begin(Subtraction &) {}
        void end(Subtraction &) {}

        void begin(Multiplication &) {}
        void end(Multiplication &) {}

        void begin(Division &) {}
        void end(Division &) {}

        void begin(IntegerLiteral &) {}
        void end(IntegerLiteral &) {}

        void begin(RealLiteral &) {}
        void end(RealLiteral &) {}

        void begin(Call &call) {

        }
        void end(Call &) {}

        void begin(Negation &) {}
        void end(Negation &) {}

        void begin(ParenExpression &) {}
        void end(ParenExpression &) {}

        void begin(Binding &binding)
        {
            scope.push(scope.top().enter_binding(binding));
        }
        void end(Binding &binding)
        {
            auto type = ASTQueries::resolve_type(binding.body(), scope.top().symbols);
            if (!type.empty() && type[0] != '<') {
                if (type != binding.type() && binding.type() != "auto")
                    throw std::runtime_error(binding.id() + " declared " + binding.type() +
                                             ", but function body is " + type);
                binding.reset_type(type);
            }
            scope.pop();
        }

        void begin(AST::Identifier &) {}
        void end(AST::Identifier &) {}

        void begin(LetIn &letin) {
            scope.push(scope.top().enter_declarative_region(letin.bindings()));
        }
        void end(LetIn &) {
            scope.pop();
        }

        void begin(Program &p) {
            scope.push(Scope::EnterProgram(p.bindings()));
        }
        void end(Program &) {
            scope.pop();
        }

    private:
         bool transformed_ = false;
         bool has_unresolved_ = false;

         struct Scope {
             std::map<string, string> symbols;
             vector<shared_ptr<Binding>> *bindings_declarative_region;
             //vector<Binding*> visible_bindings;

             Scope enter_binding(Binding &binding) const {
                Scope ret;
                //ret.visible_bindings = visible_bindings;
                //ret.visible_bindings.push_back(&binding);
                ret.bindings_declarative_region = bindings_declarative_region;

                for (auto a : binding.arguments()) {
                    ret.symbols[a.name] = a.type;
                }
                return ret;
             }

             Scope enter_declarative_region(vector<shared_ptr<Binding>> &bindings_declarative_region) const {
                Scope ret;
                ret = *this;
                ret.bindings_declarative_region = &bindings_declarative_region;
                return ret;
             }

             static Scope EnterProgram (vector<shared_ptr<Binding>> &bindings_declarative_region) {
                Scope ret;
                ret.bindings_declarative_region = &bindings_declarative_region;
                return ret;
             }

             void instantiate(shared_ptr<Binding> generic, std::vector<string> types) {
                if (types.size() != generic->arguments().size())
                    throw std::runtime_error("wrong number of arguments in call to " + generic->id());

                shared_ptr<Binding> insta (generic->deep_copy());

                for (size_t a=0, num_args=insta->arguments().size(); a!=num_args; ++a) {
                    const auto &desired = types[a];
                    Argument &arg = insta->arguments()[a];
                    if (arg.type != "auto" && desired != arg.type)
                        throw std::runtime_error("cannot instantiate");
                    arg.type = desired;
                }

                bindings_declarative_region->push_back(insta);
                //visible_bindings.push_back(&*insta);
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
            if (ll.has_unresolved()) {
                throw std::runtime_error("program is not fully resolvable");
            }
            break;
        }
    }
    std::clog << "pass: resolve-types (" << i << "x)\n";
}


} } } }
