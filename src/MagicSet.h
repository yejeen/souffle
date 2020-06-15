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

#include "AstAbstract.h"
#include "AstAnalysis.h"
#include "AstArgument.h"
#include "AstClause.h"
#include "AstLiteral.h"
#include "AstQualifiedName.h"
#include "AstVisitor.h"
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

    bool isBound(std::string varName) const {
        return contains(boundVariables, varName);
    }

    const std::set<std::string>& getBoundVariables() const {
        return boundVariables;
    }

private:
    std::set<std::string> boundVariables{};
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
            visitDepthFirst(bc, [&](const AstAggregator& /* aggr */) {
                containsAggregators = true;
            });
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
                }
                if (auto* var = dynamic_cast<AstVariable*>(bc.getRHS())) {
                    std::set<std::string> subVars;
                    visitDepthFirst(*bc.getLHS(),
                            [&](const AstVariable& subVar) { subVars.insert(subVar.getName()); });
                    addBindingDependency(var->getName(), subVars);
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

class AdornedPredicate {
private:
    AstQualifiedName predicateName;
    std::string adornment;

public:
    AdornedPredicate(AstQualifiedName name, std::string adornment)
            : predicateName(std::move(name)), adornment(std::move(adornment)) {}

    ~AdornedPredicate() = default;

    AstQualifiedName getQualifiedName() const {
        return predicateName;
    }

    std::string getAdornment() const {
        return adornment;
    }

    friend std::ostream& operator<<(std::ostream& out, const AdornedPredicate& arg) {
        out << "(" << arg.predicateName << ", " << arg.adornment << ")";
        return out;
    }

    friend bool operator<(const AdornedPredicate& p1, const AdornedPredicate& p2) {
        if (p1.getQualifiedName() != p2.getQualifiedName()) {
            return p1.getQualifiedName() < p2.getQualifiedName();
        } else {
            return p1.getAdornment() < p2.getAdornment();
        }
    }
};

class AdornedClause {
private:
    AstClause* clause;
    std::string headAdornment;
    std::vector<std::string> bodyAdornment;
    std::vector<unsigned int> ordering;

public:
    AdornedClause(AstClause* clause, std::string headAdornment, std::vector<std::string> bodyAdornment,
            std::vector<unsigned int> ordering)
            : clause(clause), headAdornment(std::move(headAdornment)),
              bodyAdornment(std::move(bodyAdornment)), ordering(std::move(ordering)) {}

    AstClause* getClause() const {
        return clause;
    }

    std::string getHeadAdornment() const {
        return headAdornment;
    }

    std::vector<std::string> getBodyAdornment() const {
        return bodyAdornment;
    }

    std::vector<unsigned int> getOrdering() const {
        return ordering;
    }

    friend std::ostream& operator<<(std::ostream& out, const AdornedClause& arg) {
        size_t currpos = 0;
        bool firstAdded = true;
        out << arg.clause->getHead()->getQualifiedName() << "{" << arg.headAdornment << "} :- ";

        std::vector<AstLiteral*> bodyLiterals = arg.clause->getBodyLiterals();
        for (AstLiteral* literal : bodyLiterals) {
            if (auto* atom = dynamic_cast<AstAtom*>(literal)) {
                if (!firstAdded) {
                    out << ", ";
                }
                firstAdded = false;
                out << atom->getQualifiedName() << "{_}";
            } else if (auto* neg = dynamic_cast<AstNegation*>(literal)) {
                if (!firstAdded) {
                    out << ", ";
                }
                firstAdded = false;
                if (currpos < arg.bodyAdornment.size()) {
                    out << neg->getAtom()->getQualifiedName() << "{" << arg.bodyAdornment[currpos++] << "}";
                } else {
                    out << neg->getAtom()->getQualifiedName() << "{__}";
                    ++currpos;
                }
            }
        }
        out << ". [order: " << arg.ordering << "]";

        return out;
    }
};

class OldBindingStore {
private:
    std::map<std::string, std::unique_ptr<AstArgument>> originalArguments;
    std::map<std::string, std::set<std::string>> varDependencies;
    std::set<std::string> variableBoundComposites;

public:
    AstArgument* cloneOriginalArgument(const std::string& argName) const {
        return originalArguments.at(argName)->clone();
    }

    const std::set<std::string>& getVariableDependencies(const std::string& argName) const {
        return varDependencies.at(argName);
    }

    void addBinding(const std::string& newVariableName, const AstArgument* arg) {
        originalArguments[newVariableName] = std::unique_ptr<AstArgument>(arg->clone());

        // find the variable dependencies
        std::set<std::string> dependencies;
        visitDepthFirst(*arg, [&](const AstVariable& var) { dependencies.insert(var.getName()); });
        varDependencies[newVariableName] = dependencies;
    }

    void addVariableBoundComposite(const std::string& functorName) {
        variableBoundComposites.insert(functorName);
    }

    bool isVariableBoundComposite(const std::string& functorName) const {
        return (variableBoundComposites.find(functorName) != variableBoundComposites.end());
    }
};

class Adornment : public AstAnalysis {
public:
    static constexpr const char* name = "adorned-clauses";

    Adornment() : AstAnalysis(name) {}

    ~Adornment() override = default;

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& os) const override;

    const std::vector<std::vector<AdornedClause>>& getAdornedClauses() const {
        return adornmentClauses;
    }

    const std::vector<AstQualifiedName>& getRelations() const {
        return adornmentRelations;
    }

    const std::set<AstQualifiedName>& getEDB() const {
        return adornmentEdb;
    }

    const std::set<AstQualifiedName>& getIDB() const {
        return adornmentIdb;
    }

    const std::set<AstQualifiedName>& getNegatedAtoms() const {
        return negatedAtoms;
    }

    const std::set<AstQualifiedName>& getIgnoredAtoms() const {
        return ignoredAtoms;
    }

    const OldBindingStore& getBindings() const {
        return bindings;
    }

private:
    std::vector<std::vector<AdornedClause>> adornmentClauses;
    std::vector<AstQualifiedName> adornmentRelations;
    std::set<AstQualifiedName> adornmentEdb;
    std::set<AstQualifiedName> adornmentIdb;
    std::set<AstQualifiedName> negatedAtoms;
    std::set<AstQualifiedName> ignoredAtoms;
    OldBindingStore bindings;
};
}  // namespace souffle
