/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SumTypeBranches.cpp
 *
 * Calculate branch to types mapping.
 *
 ***********************************************************************/

#include "ast/analysis/SumTypeBranches.h"
#include "ast/SumType.h"
#include "ast/TranslationUnit.h"
#include "ast/Visitor.h"
#include "ast/analysis/TypeEnvironment.h"
#include "ast/analysis/TypeSystem.h"

namespace souffle {

void SumTypeBranchesAnalysis::run(const AstTranslationUnit& tu) {
    const TypeEnvironment& env = tu.getAnalysis<TypeEnvironmentAnalysis>()->getTypeEnvironment();

    visitDepthFirst(tu.getProgram()->getTypes(), [&](const AstSumType& sumType) {
        auto typeName = sumType.getQualifiedName();
        if (!env.isType(typeName)) return;

        for (auto& branch : sumType.getBranches()) {
            branchToType[branch->getName()] = &env.getType(typeName);
        }
    });
}

}  // namespace souffle
