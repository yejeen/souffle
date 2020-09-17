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
#include "ast/utility/Utils.h"
#include "ast/utility/Visitor.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace souffle::ast::transform {

sips_t ReorderLiteralsTransformer::getSipsFunction(const std::string& sipsChosen) {
    // --- Create the appropriate SIPS function ---

    // Each SIPS function has a priority metric (e.g. max-bound atoms).
    // Arguments:
    //      - a vector of atoms to choose from (nullpointers in the vector will be ignored)
    //      - the set of variables bound so far
    // Returns: the index of the atom maximising the priority metric
    sips_t getNextAtomSips;

    if (sipsChosen == "strict") {
        // Goal: choose the leftmost atom always
        getNextAtomSips = [&](std::vector<Atom*> atoms, const BindingStore& /* bindingStore */) {
            std::vector<double> cost(atoms.size());
            for (const auto* atom : atoms) {
                cost.push_back(atom == nullptr ? std::numeric_limits<double>::max() : 0);
            }
            assert(atoms.size() == cost.size() && "each atom should have exactly one cost");
            return cost;
        };
    } else if (sipsChosen == "all-bound") {
        // Goal: prioritise atoms with all arguments bound
        getNextAtomSips = [&](std::vector<Atom*> atoms, const BindingStore& bindingStore) {
            std::vector<double> cost;
            for (const auto* atom : atoms) {
                if (atom == nullptr) {
                    cost.push_back(std::numeric_limits<double>::max());
                    continue;
                }

                int arity = atom->getArity();
                int numBound = bindingStore.numBoundArguments(atom);
                cost.push_back(arity == numBound ? 0 : 1);
            }
            assert(atoms.size() == cost.size() && "each atom should have exactly one cost");
            return cost;
        };
    } else if (sipsChosen == "naive") {
        // Goal: prioritise (1) all bound, then (2) atom with at least one bound argument, then (3) left-most
        getNextAtomSips = [&](std::vector<Atom*> atoms, const BindingStore& bindingStore) {
            std::vector<double> cost;
            for (const auto* atom : atoms) {
                if (atom == nullptr) {
                    cost.push_back(std::numeric_limits<double>::max());
                    continue;
                }

                int arity = atom->getArity();
                int numBound = bindingStore.numBoundArguments(atom);
                if (arity == numBound) {
                    cost.push_back(0);
                } else if (numBound >= 1) {
                    cost.push_back(1);
                } else {
                    cost.push_back(2);
                }
            }
            assert(atoms.size() == cost.size() && "each atom should have exactly one cost");
            return cost;
        };
    } else if (sipsChosen == "max-bound") {
        // Goal: prioritise (1) all-bound, then (2) max number of bound vars, then (3) left-most
        getNextAtomSips = [&](std::vector<Atom*> atoms, const BindingStore& bindingStore) {
            std::vector<double> cost;
            for (const auto* atom : atoms) {
                if (atom == nullptr) {
                    cost.push_back(std::numeric_limits<double>::max());
                    continue;
                }

                int arity = atom->getArity();
                int numBound = bindingStore.numBoundArguments(atom);
                if (arity == numBound) {
                    // Always better than anything else
                    cost.push_back(0);
                } else if (numBound == 0) {
                    // Always worse than any number of bound vars
                    cost.push_back(2);
                } else {
                    // Between 0 and 1, decreasing with more num bound
                    cost.push_back(1 / numBound);
                }
            }
            assert(atoms.size() == cost.size() && "each atom should have exactly one cost");
            return cost;
        };
    } else if (sipsChosen == "max-ratio") {
        // Goal: prioritise max ratio of bound args
        getNextAtomSips = [&](std::vector<Atom*> atoms, const BindingStore& bindingStore) {
            std::vector<double> cost;
            for (const auto* atom : atoms) {
                if (atom == nullptr) {
                    cost.push_back(std::numeric_limits<double>::max());
                    continue;
                }

                int arity = atom->getArity();
                int numBound = bindingStore.numBoundArguments(atom);
                if (arity == 0) {
                    // Always better than anything else
                    cost.push_back(0);
                } else if (numBound == 0) {
                    // Always worse than anything else
                    cost.push_back(2);
                } else {
                    // Between 0 and 1, decreasing as the ratio increases
                    cost.push_back(1 - numBound / arity);
                }
            }
            assert(atoms.size() == cost.size() && "each atom should have exactly one cost");
            return cost;
        };
    } else if (sipsChosen == "least-free") {
        // Goal: choose the atom with the least number of unbound arguments
        getNextAtomSips = [&](std::vector<Atom*> atoms, const BindingStore& bindingStore) {
            std::vector<double> cost;
            for (const auto* atom : atoms) {
                if (atom == nullptr) {
                    cost.push_back(std::numeric_limits<double>::max());
                    continue;
                }

                cost.push_back(atom->getArity() - bindingStore.numBoundArguments(atom));
            }
            return cost;
        };
    } else if (sipsChosen == "least-free-vars") {
        // Goal: choose the atom with the least amount of unbound variables
        getNextAtomSips = [&](std::vector<Atom*> atoms, const BindingStore& bindingStore) {
            std::vector<double> cost;
            for (const auto* atom : atoms) {
                if (atom == nullptr) {
                    cost.push_back(std::numeric_limits<double>::max());
                    continue;
                }

                // use a set to hold all free variables to avoid double-counting
                std::set<std::string> freeVars;
                visitDepthFirst(*atom, [&](const Variable& var) {
                    if (bindingStore.isBound(var.getName())) {
                        freeVars.insert(var.getName());
                    }
                });
                cost.push_back(freeVars.size());
            }
            return cost;
        };
    } else if (sipsChosen == "ast2ram") {
        // TEMP: all-bound
        return getSipsFunction("all-bound");
    } else {
        // Default is strict - unchanged
        return getSipsFunction("strict");
    }

    return getNextAtomSips;
}

std::vector<unsigned int> ReorderLiteralsTransformer::getOrderingAfterSIPS(
        sips_t sipsFunction, const Clause* clause) {
    BindingStore bindingStore(clause);
    auto atoms = getBodyLiterals<Atom>(*clause);
    std::vector<unsigned int> newOrder(atoms.size());

    unsigned int numAdded = 0;
    while (numAdded < atoms.size()) {
        // grab the index of the next atom, based on the SIPS function
        const auto& costs = sipsFunction(atoms, bindingStore);
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

Clause* ReorderLiteralsTransformer::reorderClauseWithSips(sips_t sipsFunction, const Clause* clause) {
    // ignore clauses with fixed execution plans
    if (clause->getExecutionPlan() != nullptr) {
        return nullptr;
    }

    // get the ordering corresponding to the SIPS
    std::vector<unsigned int> newOrdering = getOrderingAfterSIPS(sipsFunction, clause);

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
    auto sipsFunction = getSipsFunction(sipsChosen);

    // literal reordering is a rule-local transformation
    std::vector<Clause*> clausesToRemove;

    for (Clause* clause : program.getClauses()) {
        Clause* newClause = reorderClauseWithSips(sipsFunction, clause);
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
        auto* profileUse = translationUnit.getAnalysis<analysis::ProfileUseAnalysis>();

        auto profilerSips = [&](std::vector<Atom*> atoms, const BindingStore& bindingStore) {
            // Goal: reorder based on the given profiling information
            // Metric: cost(atom_R) = log(|atom_R|) * #free/#args
            //         - exception: propositions are prioritised

            std::vector<double> cost;
            for (const auto* atom : atoms) {
                if (atom == nullptr) {
                    cost.push_back(std::numeric_limits<double>::max());
                    continue;
                }

                // prioritise propositions
                int arity = atom->getArity();
                if (arity == 0) {
                    cost.push_back(0);
                    continue;
                }

                // calculate log(|R|) * #free/#args
                int numBound = bindingStore.numBoundArguments(atom);
                int numFree = arity - numBound;
                double value = log(profileUse->getRelationSize(atom->getQualifiedName()));
                value *= (numFree * 1.0) / arity;
            }
            return cost;
        };

        // change the ordering of literals within clauses
        std::vector<Clause*> clausesToRemove;

        for (Clause* clause : program.getClauses()) {
            Clause* newClause = reorderClauseWithSips(profilerSips, clause);
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
