// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "lambda_lift.hh"
#include "../ASTDumper.hh"

namespace excyrender { namespace Nature { namespace Et1 { namespace AST {



struct LambdaLift final : Visitor {
    LambdaLift(shared_ptr<Program> &out) : out(out) {}

    void begin(Addition const &) {}
    void end(Addition const &) {}

    void begin(Subtraction const &) {}
    void end(Subtraction const &) {}

    void begin(Multiplication const &) {}
    void end(Multiplication const &) {}

    void begin(Division const &) {}
    void end(Division const &) {}

    void begin(IntegerLiteral const &) {}
    void end(IntegerLiteral const &) {}

    void begin(Call const &) {}
    void end(Call const &) {}

    void begin(Negation const &) {}
    void end(Negation const &) {}

    void begin(ParenExpression const &) {}
    void end(ParenExpression const &) {}

    void begin(Binding const &) {}
    void end(Binding const &) {}

    void begin(Identifier const &) {}
    void end(Identifier const &) {}

    void begin(LetIn const &) {}
    void end(LetIn const &) {}

    void begin(Program const &prog)
    {
        out.reset(new Program(prog));
    }

    void end(Program const &) {}

private:
    shared_ptr<Program> &out;
};



shared_ptr<Program> lambda_lift(shared_ptr<Program> in) {
    std::cout << "--- lambda lifting --------------------------------------------\n";
    ASTDumper d;
    in->accept(d);

    shared_ptr<Program> out;
    LambdaLift ll(out);
    in->accept(ll);

    std::cout << "---------------------------------------------------------------\n";

    out->accept(d);

    std::cout << "---------------------------------------------------------------\n";
    return out;
}


} } } }
