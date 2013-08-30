// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef UNITTESTING_HH_20130822
#define UNITTESTING_HH_20130822

#include "AST.hh"
#include "catch.hpp"

namespace excyrender { namespace Nature { namespace Et1 { namespace detail {

    std::shared_ptr<AST::Program> to_ast(std::string const &code);
    bool equal (std::string const &in, std::string const &expected,
                std::function<void (std::shared_ptr<excyrender::Nature::Et1::AST::Program>)> pass);

} } } }


#endif // UNITTESTING_HH_20130822
