/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file MagicSet.h
 *
 * Define classes and functionality related to the magic set transformation.
 *
 ***********************************************************************/

#pragma once

#include "ast/AstAbstract.h"
#include "ast/AstArgument.h"
#include "ast/AstClause.h"
#include "ast/AstLiteral.h"
#include "ast/AstQualifiedName.h"
#include "ast/AstVisitor.h"
#include "ast/analysis/AstAnalysis.h"
#include "utility/StreamUtil.h"
#include <cstddef>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

class AstTranslationUnit;

class BindingStore {
public:
    BindingStore(const AstClause* clause) {
        generateBindingDependencies(clause);
        reduceDependencies();
    }

    void bindVariable(std::string varName) {
        boundVariables.insert(varName);
        reduceDependencies();
    }

    void bindHeadVariable(std::string varName) {
        boundHeadVariables.insert(varName);
    }

    bool isBound(std::string varName) const {
        return contains(boundVariables, varName) || contains(boundHeadVariables, varName);
    }

    const std::set<std::string>& getBoundVariables() const {
        return boundVariables;
    }

private:
    std::set<std::string> boundVariables{};
    std::set<std::string> boundHeadVariables{};
    std::map<std::string, std::set<std::set<std::string>>> bindingDependencies{};

    void generateBindingDependencies(const AstClause* clause) {
        auto addBindingDependency = [&](std::string variable, std::set<std::string> dependency) {
            if (contains(bindingDependencies, variable)) {
                bindingDependencies[variable].insert(dependency);
            } else {
                std::set<std::set<std::string>> dependencies;
                dependencies.insert(dependency);
                bindingDependencies[variable] = dependencies;
            }
        };

        visitDepthFirst(*clause, [&](const AstBinaryConstraint& bc) {
            // Ignore binary constraints involving aggregators
            bool containsAggregators = false;
            visitDepthFirst(bc, [&](const AstAggregator& /* aggr */) { containsAggregators = true; });
            if (containsAggregators) {
                return;
            }

            // Add variable binding dependencies
            if (bc.getOperator() == BinaryConstraintOp::EQ) {
                if (auto* var = dynamic_cast<AstVariable*>(bc.getLHS())) {
                    std::set<std::string> subVars;
                    visitDepthFirst(*bc.getRHS(),
                            [&](const AstVariable& subVar) { subVars.insert(subVar.getName()); });
                    addBindingDependency(var->getName(), subVars);
                    if (const auto* rec = dynamic_cast<const AstRecordInit*>(bc.getRHS())) {
                        for (const auto* arg : rec->getArguments()) {
                            if (const auto* subVar = dynamic_cast<const AstVariable*>(arg)) {
                                std::set<std::string> singletonVar;
                                singletonVar.insert(var->getName());
                                addBindingDependency(subVar->getName(), singletonVar);
                            }
                        }
                    }
                }
                if (auto* var = dynamic_cast<AstVariable*>(bc.getRHS())) {
                    std::set<std::string> subVars;
                    visitDepthFirst(*bc.getLHS(),
                            [&](const AstVariable& subVar) { subVars.insert(subVar.getName()); });
                    addBindingDependency(var->getName(), subVars);
                    if (const auto* rec = dynamic_cast<const AstRecordInit*>(bc.getLHS())) {
                        for (const auto* arg : rec->getArguments()) {
                            if (const auto* subVar = dynamic_cast<const AstVariable*>(arg)) {
                                std::set<std::string> singletonVar;
                                singletonVar.insert(var->getName());
                                addBindingDependency(subVar->getName(), singletonVar);
                            }
                        }
                    }
                }
            }
        });
    }

    bool reduceDependencies() {
        bool changed = false;
        std::map<std::string, std::set<std::set<std::string>>> newBindingDependencies;
        std::set<std::string> variablesToBind;

        for (auto& pair : bindingDependencies) {
            auto headVar = pair.first;
            if (contains(boundVariables, headVar)) {
                // No need to add the dependencies of already-bound variables
                changed = true;
                continue;
            }

            const auto& dependencies = pair.second;
            assert(!dependencies.empty() &&
                    "a variable is only added if it appears in >= 1 binary constraint");

            bool nowBound = false;
            std::set<std::set<std::string>> newDependencies;
            for (const auto& dep : dependencies) {
                if (dep.empty()) {
                    // Dependency satisfied!
                    nowBound = true;
                    changed = true;
                    break;
                }

                // Only keep unbound variables in the dependency
                std::set<std::string> newDependency;
                for (const auto& var : dep) {
                    if (!contains(boundVariables, var)) {
                        newDependency.insert(var);
                    } else {
                        changed = true;
                    }
                }

                newDependencies.insert(newDependency);
            }

            if (nowBound) {
                // Dependency has been satisfied
                variablesToBind.insert(headVar);
            } else {
                // Dependencies not satisfied yet, keep them in store
                newBindingDependencies[headVar] = newDependencies;
            }
        }

        for (auto var : variablesToBind) {
            boundVariables.insert(var);
        }

        if (changed) {
            bindingDependencies = newBindingDependencies;
            reduceDependencies();
            return true;
        }

        assert(bindingDependencies == newBindingDependencies && "unexpected change");
        return false;
    }
};

}  // namespace souffle
