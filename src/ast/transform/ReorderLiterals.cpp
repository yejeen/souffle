/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ReorderLiterals.cpp
 *
 * Define classes and functionality related to the ReorderLiterals
 * transformer.
 *
 ***********************************************************************/

#include "ast/transform/ReorderLiterals.h"
#include "Global.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/Clause.h"
#include "ast/Program.h"
#include "ast/TranslationUnit.h"
#include "ast/Variable.h"
#include "ast/analysis/ProfileUse.h"
#include "ast/utility/BindingStore.h"
#include "ast/utility/SipsMetric.h"
#include "ast/utility/Utils.h"
#include <algorithm>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace souffle::ast::transform {

std::unique_ptr<SipsMetric> ReorderLiteralsTransformer::getSipsFunction(
        const std::string& sipsChosen, const TranslationUnit& tu) {
    if (sipsChosen == "strict")
        return std::make_unique<StrictSips>();
    else if (sipsChosen == "all-bound")
        return std::make_unique<AllBoundSips>();
    else if (sipsChosen == "naive")
        return std::make_unique<NaiveSips>();
    else if (sipsChosen == "max-bound")
        return std::make_unique<MaxBoundSips>();
    else if (sipsChosen == "max-ratio")
        return std::make_unique<MaxRatioSips>();
    else if (sipsChosen == "least-free")
        return std::make_unique<LeastFreeSips>();
    else if (sipsChosen == "least-free-vars")
        return std::make_unique<LeastFreeVarsSips>();
    else if (sipsChosen == "profile-use")
        return std::make_unique<ProfileUseSips>(*tu.getAnalysis<analysis::ProfileUseAnalysis>());

    // default is all-bound
    return getSipsFunction("all-bound", tu);
}

Clause* ReorderLiteralsTransformer::reorderClauseWithSips(const SipsMetric& sips, const Clause* clause) {
    // ignore clauses with fixed execution plans
    if (clause->getExecutionPlan() != nullptr) {
        return nullptr;
    }

    // get the ordering corresponding to the SIPS
    std::vector<unsigned int> newOrdering = sips.getReordering(clause);

    // check if we need a change
    bool changeNeeded = false;
    for (unsigned int i = 0; i < newOrdering.size(); i++) {
        if (newOrdering[i] != i) {
            changeNeeded = true;
        }
    }

    // reorder if needed
    return changeNeeded ? reorderAtoms(clause, newOrdering) : nullptr;
}

bool ReorderLiteralsTransformer::transform(TranslationUnit& translationUnit) {
    bool changed = false;
    Program& program = *translationUnit.getProgram();

    // --- SIPS-based static reordering ---
    // ordering is based on the given SIPS

    // default SIPS to choose is 'all-bound'
    std::string sipsChosen = "all-bound";
    if (Global::config().has("SIPS")) {
        sipsChosen = Global::config().get("SIPS");
    }
    auto sipsFunction = getSipsFunction(sipsChosen, translationUnit);

    // literal reordering is a rule-local transformation
    std::vector<Clause*> clausesToRemove;

    for (Clause* clause : program.getClauses()) {
        Clause* newClause = reorderClauseWithSips(*sipsFunction, clause);
        if (newClause != nullptr) {
            // reordering needed - swap around
            clausesToRemove.push_back(clause);
            program.addClause(Own<Clause>(newClause));
        }
    }

    changed |= !clausesToRemove.empty();
    for (auto* clause : clausesToRemove) {
        program.removeClause(clause);
    }

    // --- profile-guided reordering ---
    if (Global::config().has("profile-use")) {
        // parse supplied profile information
        auto profilerSips = getSipsFunction("profiler", translationUnit);

        // change the ordering of literals within clauses
        std::vector<Clause*> clausesToRemove;
        for (Clause* clause : program.getClauses()) {
            Clause* newClause = reorderClauseWithSips(*profilerSips, clause);
            if (newClause != nullptr) {
                // reordering needed - swap around
                clausesToRemove.push_back(clause);
                program.addClause(Own<Clause>(newClause));
            }
        }

        changed |= !clausesToRemove.empty();
        for (auto* clause : clausesToRemove) {
            program.removeClause(clause);
        }
    }

    return changed;
}

}  // namespace souffle::ast::transform
