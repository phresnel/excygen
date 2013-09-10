// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef ASTALGORITHM_HH_INCLUDED_20130910
#define ASTALGORITHM_HH_INCLUDED_20130910

#include "AST.hh"

namespace excyrender { namespace Nature { namespace Et1 { namespace Algorithm {

void transform_expressions (AST::ASTNode &ast, std::function<void (shared_ptr<AST::Expression>&)> t);

} } } }

#endif // ASTALGORITHM_HH_INCLUDED_20130910
