/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SipsMetric.cpp
 *
 * Defines the SipsMetric class, which specifies cost functions for atom orderings in a clause.
 *
 ***********************************************************************/

#include "ast/utility/SipsMetric.h"
#include "ast/Clause.h"
#include "ast/Variable.h"
#include "ast/utility/BindingStore.h"
#include "ast/utility/Utils.h"
#include <vector>

namespace souffle::ast {

// TODO: change clause -> atom vector
std::vector<unsigned int> SipsMetric::getReordering(const Clause* clause) const {
    BindingStore bindingStore(clause);
    auto atoms = getBodyLiterals<Atom>(*clause);
    std::vector<unsigned int> newOrder(atoms.size());

    size_t numAdded = 0;
    while (numAdded < atoms.size()) {
        // grab the index of the next atom, based on the SIPS function
        const auto& costs = evaluateCosts(atoms, bindingStore);
        auto minIdx = std::distance(costs.begin(), std::min_element(costs.begin(), costs.end()));
        const auto* nextAtom = atoms[minIdx];
        assert(nextAtom != nullptr && "nullptr atoms should have maximal cost");

        // set all arguments that are variables as bound
        for (const auto* arg : nextAtom->getArguments()) {
            if (const auto* var = dynamic_cast<const Variable*>(arg)) {
                bindingStore.bindVariableStrongly(var->getName());
            }
        }

        newOrder[numAdded] = minIdx;  // add to the ordering
        atoms[minIdx] = nullptr;      // mark as done
        numAdded++;                   // move on
    }

    return newOrder;
}

};  // namespace souffle::ast
