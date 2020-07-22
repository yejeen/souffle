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
#include "ast/transform/AstTransformer.h"
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

class AstTranslationUnit;

/**
 * Database normaliser for MST.
 * Effects:
 *  - Partitions database into [input|intermediate|queries]
 *  - Normalises all arguments and constraints
 * Prerequisite for adornment.
 */
class NormaliseDatabaseTransformer : public AstTransformer {
public:
    std::string getName() const override {
        return "NormaliseDatabaseTransformer";
    }

    NormaliseDatabaseTransformer* clone() const override {
        return new NormaliseDatabaseTransformer();
    }

private:
    bool transform(AstTranslationUnit& translationUnit) override;

    /**
     * Partitions the input and output relations.
     * Program will no longer have relations that are both input and output.
     */
    static bool partitionIO(AstTranslationUnit& translationUnit);

    /**
     * Separates the IDB from the EDB, so that they are disjoint.
     * Program will no longer have input relations that appear as the head of clauses.
     */
    static bool extractIDB(AstTranslationUnit& translationUnit);

    /**
     * Extracts output relations into separate simple query relations,
     * so that they are unused in any other rules.
     * Programs will only contain output relations which:
     *      (1) have exactly one rule defining them
     *      (2) do not appear in other rules
     */
    static bool querifyOutputRelations(AstTranslationUnit& translationUnit);

    /**
     * Normalise all arguments within each clause.
     * All arguments in all clauses will now be either:
     *      (1) a variable, or
     *      (2) the RHS of a `<var> = <arg>` constraint
     */
    static bool normaliseArguments(AstTranslationUnit& translationUnit);
};

/**
 * Database labeller. Runs the magic-set labelling algorithm.
 * Necessary for supporting negation in MST.
 */
class LabelDatabaseTransformer : public AstTransformer {
public:
    std::string getName() const override {
        return "LabelDatabaseTransformer";
    }

    LabelDatabaseTransformer* clone() const override {
        return new LabelDatabaseTransformer();
    }

private:
    bool transform(AstTranslationUnit& translationUnit) override;

    /**
     * Runs the first stage of the labelling algorithm.
     * Separates out negated appearances of relations from the main SCC graph, preventing
     * them from affecting stratification once magic dependencies are added.
     */
    static bool runNegativeLabelling(AstTranslationUnit& translationUnit);

    /**
     * Runs the second stage of the labelling algorithm.
     * Separates out the dependencies of negatively labelled atoms from the main SCC
     * graph, preventing them from affecting stratification after magic.
     * Negative labelling must have been run first.
     */
    static bool runPositiveLabelling(AstTranslationUnit& translationUnit);

    /**
     * Provide a unique name for negatively-labelled relations.
     */
    static AstQualifiedName getNegativeLabel(const AstQualifiedName& name);
};

/**
 * Database adornment.
 * Adorns the rules of a database with variable flow and binding information.
 * Prerequisite for the magic set transformation.
 */
class AdornDatabaseTransformer : public AstTransformer {
public:
    std::string getName() const override {
        return "AdornDatabaseTransformer";
    }

    AdornDatabaseTransformer* clone() const override {
        return new AdornDatabaseTransformer();
    }

private:
    using adorned_predicate = std::pair<AstQualifiedName, std::string>;

    std::set<adorned_predicate> headAdornmentsToDo;
    std::set<AstQualifiedName> headAdornmentsSeen;

    std::vector<std::unique_ptr<AstClause>> adornedClauses;
    std::vector<std::unique_ptr<AstClause>> redundantClauses;
    std::set<AstQualifiedName> relationsToIgnore;

    bool transform(AstTranslationUnit& translationUnit) override;

    static AstQualifiedName getAdornmentID(const AstQualifiedName& relName, const std::string& adornmentMarker);

    void queueAdornment(const AstQualifiedName& relName, const std::string& adornmentMarker) {
        auto adornmentID = getAdornmentID(relName, adornmentMarker);
        if (!contains(headAdornmentsSeen, adornmentID)) {
            headAdornmentsToDo.insert(std::make_pair(relName, adornmentMarker));
            headAdornmentsSeen.insert(adornmentID);
        }
    }

    bool hasAdornmentToProcess() const {
        return !headAdornmentsToDo.empty();
    }

    adorned_predicate nextAdornmentToProcess() {
        assert(hasAdornmentToProcess() && "no adornment to pop");
        auto headAdornment = *(headAdornmentsToDo.begin());
        headAdornmentsToDo.erase(headAdornmentsToDo.begin());
        return headAdornment;
    }

    std::unique_ptr<AstClause> adornClause(const AstClause* clause, const std::string& adornmentMarker);
};

/**
 * Magic Set Transformation.
 * Before running this transformation, need to run:
 *      (1) NormaliseDatabaseTransformer, for assumptions to hold
 *      (2) LabelDatabaseTransformer, to support negation
 *      (3) AdornDatabaseTransformer, to annotate information flow
 */
class MagicSetTransformer : public AstTransformer {
public:
    std::string getName() const override {
        return "MagicSetTransformer";
    }

    MagicSetTransformer* clone() const override {
        return new MagicSetTransformer();
    }

private:
    bool transform(AstTranslationUnit& translationUnit) override;
};

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
