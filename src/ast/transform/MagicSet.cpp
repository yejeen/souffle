/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file MagicSet.cpp
 *
 * Define classes and functionality related to the magic set transformation.
 *
 ***********************************************************************/

#include "ast/transform/MagicSet.h"
#include "BinaryConstraintOps.h"
#include "Global.h"
#include "MagicSet.h"
#include "RamTypes.h"
#include "SrcLocation.h"
#include "ast/AstAttribute.h"
#include "ast/AstIO.h"
#include "ast/AstNode.h"
#include "ast/AstProgram.h"
#include "ast/AstQualifiedName.h"
#include "ast/AstRelation.h"
#include "ast/AstTranslationUnit.h"
#include "ast/AstUtils.h"
#include "ast/analysis/AstIOTypeAnalysis.h"
#include "ast/transform/AstTransforms.h"
#include "utility/ContainerUtil.h"
#include "utility/MiscUtil.h"
#include "utility/StringUtil.h"
#include <utility>

namespace souffle {

bool NormaliseDatabaseTransformer::transform(AstTranslationUnit& translationUnit) {
    bool changed = false;

    /** (1) Partition input and output relations */
    changed |= partitionIO(translationUnit);
    if (changed) translationUnit.invalidateAnalyses();

    /** (2) Separate the IDB from the EDB */
    changed |= extractIDB(translationUnit);
    if (changed) translationUnit.invalidateAnalyses();

    /** (3) Move constants into new equality constraints */
    changed |= nameConstants(translationUnit);
    if (changed) translationUnit.invalidateAnalyses();

    /** (4) Querify output relations */
    changed |= querifyOutputRelations(translationUnit);
    if (changed) translationUnit.invalidateAnalyses();

    return changed;
}

bool NormaliseDatabaseTransformer::partitionIO(AstTranslationUnit& translationUnit) {
    auto* ioTypes = translationUnit.getAnalysis<IOType>();
    auto& program = *translationUnit.getProgram();

    std::set<AstQualifiedName> relationsToSplit;
    for (auto* rel : program.getRelations()) {
        if (ioTypes->isInput(rel) && (ioTypes->isOutput(rel) || ioTypes->isPrintSize(rel))) {
            relationsToSplit.insert(rel->getQualifiedName());
        }
    }

    for (auto relName : relationsToSplit) {
        const auto* rel = getRelation(program, relName);
        assert(rel != nullptr && "relation does not exist");
        auto newRelName = AstQualifiedName(relName);
        newRelName.prepend("@split_in");

        // Create a new intermediate input relation
        auto newRelation = std::make_unique<AstRelation>(newRelName);
        for (const auto* attr : rel->getAttributes()) {
            newRelation->addAttribute(std::unique_ptr<AstAttribute>(attr->clone()));
        }

        // Read in the input relation into the original relation
        auto newClause = std::make_unique<AstClause>();
        auto newHeadAtom = std::make_unique<AstAtom>(relName);
        auto newBodyAtom = std::make_unique<AstAtom>(newRelName);
        for (size_t i = 0; i < rel->getArity(); i++) {
            std::stringstream varName;
            varName << "@var" << i;
            newHeadAtom->addArgument(std::make_unique<AstVariable>(varName.str()));
            newBodyAtom->addArgument(std::make_unique<AstVariable>(varName.str()));
        }
        newClause->setHead(std::move(newHeadAtom));
        newClause->addToBody(std::move(newBodyAtom));

        // New relation should be input, original should not
        std::set<const AstIO*> iosToDelete;
        std::set<std::unique_ptr<AstIO>> iosToAdd;
        for (const auto* io : program.getIOs()) {
            if (io->getQualifiedName() == relName && io->getType() == AstIoType::input) {
                auto newIO = std::unique_ptr<AstIO>(io->clone());
                newIO->setQualifiedName(newRelName);
                iosToAdd.insert(std::move(newIO));
                iosToDelete.insert(io);
            }
        }

        for (const auto* io : iosToDelete) {
            program.removeIO(io);
        }
        for (auto& io : iosToAdd) {
            program.addIO(std::unique_ptr<AstIO>(io->clone()));
        }

        program.addRelation(std::move(newRelation));
        program.addClause(std::move(newClause));
    }

    return !relationsToSplit.empty();
}

bool NormaliseDatabaseTransformer::extractIDB(AstTranslationUnit& translationUnit) {
    auto* ioTypes = translationUnit.getAnalysis<IOType>();
    auto& program = *translationUnit.getProgram();

    auto isStrictlyIDB = [&](const AstRelation* rel) {
        bool hasRules = false;
        for (const auto* clause : getClauses(program, rel->getQualifiedName())) {
            visitDepthFirst(clause->getBodyLiterals(), [&](const AstAtom& /* atom */) { hasRules = true; });
        }
        return !hasRules;
    };

    // Get all input relations
    std::set<AstQualifiedName> inputRelationNames;
    std::set<AstRelation*> inputRelations;
    for (auto* rel : program.getRelations()) {
        if (ioTypes->isInput(rel) && !isStrictlyIDB(rel)) {
            auto name = rel->getQualifiedName();
            auto usedName = rel->getQualifiedName();
            usedName.prepend("@interm_in");

            auto* newRelation = rel->clone();
            newRelation->setQualifiedName(usedName);
            program.addRelation(std::unique_ptr<AstRelation>(newRelation));

            inputRelations.insert(rel);
            inputRelationNames.insert(name);
        }
    }

    // Rename them systematically
    struct rename_relation : public AstNodeMapper {
        const std::set<AstQualifiedName>& relations;

        rename_relation(const std::set<AstQualifiedName>& relations) : relations(relations) {}

        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            if (auto* atom = dynamic_cast<AstAtom*>(node.get())) {
                if (contains(relations, atom->getQualifiedName())) {
                    auto newName = atom->getQualifiedName();
                    newName.prepend("@interm_in");
                    auto* renamedAtom = atom->clone();
                    renamedAtom->setQualifiedName(newName);
                    return std::unique_ptr<AstAtom>(renamedAtom);
                }
            }
            node->apply(*this);
            return node;
        }
    };
    rename_relation update(inputRelationNames);
    program.apply(update);

    // Add the new simple query output relations
    for (auto* rel : inputRelations) {
        auto name = rel->getQualifiedName();
        auto newName = rel->getQualifiedName();
        newName.prepend("@interm_in");

        auto queryHead = std::make_unique<AstAtom>(newName);
        auto queryLiteral = std::make_unique<AstAtom>(name);
        for (size_t i = 0; i < rel->getArity(); i++) {
            std::stringstream var;
            var << "@query_x" << i;
            queryHead->addArgument(std::make_unique<AstVariable>(var.str()));
            queryLiteral->addArgument(std::make_unique<AstVariable>(var.str()));
        }
        auto query = std::make_unique<AstClause>(std::move(queryHead));
        query->addToBody(std::move(queryLiteral));
        program.addClause(std::move(query));
    }

    return !inputRelationNames.empty();
}

bool NormaliseDatabaseTransformer::nameConstants(AstTranslationUnit& translationUnit) {
    auto& program = *translationUnit.getProgram();

    // Replace all constants and underscores with named variables
    struct constant_normaliser : public AstNodeMapper {
        std::set<std::unique_ptr<AstBinaryConstraint>>& constraints;
        int& changeCount;

        constant_normaliser(std::set<std::unique_ptr<AstBinaryConstraint>>& constraints, int& changeCount)
                : constraints(constraints), changeCount(changeCount) {}

        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            node->apply(*this);
            if (auto* arg = dynamic_cast<AstArgument*>(node.get())) {
                if (dynamic_cast<AstVariable*>(arg) == nullptr) {
                    std::stringstream name;
                    name << "@abdul" << changeCount++;
                    if (dynamic_cast<AstUnnamedVariable*>(arg) == nullptr) {
                        constraints.insert(std::make_unique<AstBinaryConstraint>(BinaryConstraintOp::EQ,
                                std::make_unique<AstVariable>(name.str()),
                                std::unique_ptr<AstArgument>(arg->clone())));
                    }
                    return std::make_unique<AstVariable>(name.str());
                }
            }
            return node;
        }
    };

    bool changed = false;
    for (auto* clause : program.getClauses()) {
        int changeCount = 0;
        std::set<std::unique_ptr<AstBinaryConstraint>> constraintsToAdd;
        constant_normaliser update(constraintsToAdd, changeCount);
        clause->getHead()->apply(update);
        for (AstLiteral* lit : clause->getBodyLiterals()) {
            if (auto* bc = dynamic_cast<AstBinaryConstraint*>(lit)) {
                if (bc->getOperator() == BinaryConstraintOp::EQ &&
                        dynamic_cast<AstVariable*>(bc->getLHS()) != nullptr) {
                    continue;
                }
            }
            lit->apply(update);
        }
        visitDepthFirst(*clause, [&](const AstAtom& atom) { const_cast<AstAtom&>(atom).apply(update); });
        changed |= changeCount != 0;
        for (auto& constraint : constraintsToAdd) {
            clause->addToBody(std::unique_ptr<AstLiteral>(constraint->clone()));
        }
    }

    return changed;
}

bool NormaliseDatabaseTransformer::querifyOutputRelations(AstTranslationUnit& translationUnit) {
    auto& program = *translationUnit.getProgram();

    auto isStrictlyOutput = [&](const AstRelation* rel) {
        bool strictlyOutput = true;
        size_t ruleCount = 0;

        for (const auto* clause : program.getClauses()) {
            visitDepthFirst(clause->getBodyLiterals(), [&](const AstAtom& atom) {
                if (atom.getQualifiedName() == rel->getQualifiedName()) {
                    strictlyOutput = false;
                }
            });
            if (clause->getHead()->getQualifiedName() == rel->getQualifiedName()) {
                ruleCount++;
            }
        }

        return strictlyOutput && ruleCount <= 1;
    };

    // Get all output relations
    auto* ioTypes = translationUnit.getAnalysis<IOType>();
    std::set<AstQualifiedName> outputRelationNames;
    std::set<AstRelation*> outputRelations;
    for (auto* rel : program.getRelations()) {
        if ((ioTypes->isOutput(rel) || ioTypes->isPrintSize(rel)) && !isStrictlyOutput(rel)) {
            auto name = rel->getQualifiedName();
            auto queryName = rel->getQualifiedName();
            queryName.prepend("@interm_out");

            auto* newRelation = rel->clone();
            newRelation->setQualifiedName(queryName);
            program.addRelation(std::unique_ptr<AstRelation>(newRelation));

            outputRelations.insert(rel);
            outputRelationNames.insert(name);
        }
    }

    // Rename them systematically
    struct rename_relation : public AstNodeMapper {
        const std::set<AstQualifiedName>& relations;

        rename_relation(const std::set<AstQualifiedName>& relations) : relations(relations) {}

        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            if (auto* atom = dynamic_cast<AstAtom*>(node.get())) {
                if (contains(relations, atom->getQualifiedName())) {
                    auto newName = atom->getQualifiedName();
                    newName.prepend("@interm_out");
                    auto* renamedAtom = atom->clone();
                    renamedAtom->setQualifiedName(newName);
                    return std::unique_ptr<AstAtom>(renamedAtom);
                }
            }
            node->apply(*this);
            return node;
        }
    };
    rename_relation update(outputRelationNames);
    program.apply(update);

    // Add the new simple query output relations
    for (auto* rel : outputRelations) {
        auto name = rel->getQualifiedName();
        auto newName = rel->getQualifiedName();
        newName.prepend("@interm_out");

        auto queryHead = std::make_unique<AstAtom>(name);
        auto queryLiteral = std::make_unique<AstAtom>(newName);
        for (size_t i = 0; i < rel->getArity(); i++) {
            std::stringstream var;
            var << "@query_x" << i;
            queryHead->addArgument(std::make_unique<AstVariable>(var.str()));
            queryLiteral->addArgument(std::make_unique<AstVariable>(var.str()));
        }
        auto query = std::make_unique<AstClause>(std::move(queryHead));
        query->addToBody(std::move(queryLiteral));
        program.addClause(std::move(query));
    }

    return !outputRelationNames.empty();
}

std::set<AstQualifiedName> AdornDatabaseTransformer::getIgnoredRelations(
        AstTranslationUnit& translationUnit) {
    auto& program = *translationUnit.getProgram();
    auto* ioTypes = translationUnit.getAnalysis<IOType>();

    std::set<AstQualifiedName> relationsToIgnore;

    // - Any relations not specified to magic-set
    std::vector<std::string> specifiedRelations = splitString(Global::config().get("magic-transform"), ',');
    if (!contains(specifiedRelations, "*")) {
        for (const AstRelation* rel : program.getRelations()) {
            if (!contains(specifiedRelations, toString(rel->getQualifiedName()))) {
                relationsToIgnore.insert(rel->getQualifiedName());
            }
        }
    }

    // - Any relations known in constant time (IDB relations)
    for (auto* rel : program.getRelations()) {
        // Input relations
        if (ioTypes->isInput(rel)) {
            relationsToIgnore.insert(rel->getQualifiedName());
            continue;
        }

        // Any relations not dependent on any atoms
        bool hasRules = false;
        for (const auto* clause : getClauses(program, rel->getQualifiedName())) {
            visitDepthFirst(clause->getBodyLiterals(), [&](const AstAtom& /* atom */) { hasRules = true; });
        }
        if (!hasRules) {
            relationsToIgnore.insert(rel->getQualifiedName());
        }
    }

    // - Any relation with a neglabel
    visitDepthFirst(program, [&](const AstAtom& atom) {
        const auto& qualifiers = atom.getQualifiedName().getQualifiers();
        if (!qualifiers.empty() && qualifiers[0] == "@neglabel") {
            relationsToIgnore.insert(atom.getQualifiedName());
        }
    });

    // - Any relation with a clause containing float-related binary constraints
    const std::set<BinaryConstraintOp> floatOps(
            {BinaryConstraintOp::FEQ, BinaryConstraintOp::FNE, BinaryConstraintOp::FLE,
                    BinaryConstraintOp::FGE, BinaryConstraintOp::FLT, BinaryConstraintOp::FGT});
    for (const auto* clause : program.getClauses()) {
        visitDepthFirst(*clause, [&](const AstBinaryConstraint& bc) {
            if (contains(floatOps, bc.getOperator())) {
                relationsToIgnore.insert(clause->getHead()->getQualifiedName());
            }
        });
    }

    // - Any relation with a clause containing order-dependent functors
    const std::set<FunctorOp> orderDepFuncOps(
            {FunctorOp::MOD, FunctorOp::FDIV, FunctorOp::DIV, FunctorOp::UMOD});
    for (const auto* clause : program.getClauses()) {
        visitDepthFirst(*clause, [&](const AstIntrinsicFunctor& functor) {
            if (contains(orderDepFuncOps, functor.getFunctionInfo()->op)) {
                relationsToIgnore.insert(clause->getHead()->getQualifiedName());
            }
        });
    }

    // - Any eqrel relation
    for (auto* rel : program.getRelations()) {
        if (rel->getRepresentation() == RelationRepresentation::EQREL) {
            relationsToIgnore.insert(rel->getQualifiedName());
        }
    }

    // - Any relation with execution plans
    for (auto* clause : program.getClauses()) {
        if (clause->getExecutionPlan() != nullptr) {
            relationsToIgnore.insert(clause->getHead()->getQualifiedName());
        }
    }

    // - Any atom appearing in a clause containing a counter
    for (auto* clause : program.getClauses()) {
        bool containsCounter = false;
        visitDepthFirst(*clause, [&](const AstCounter& /* counter */) { containsCounter = true; });
        if (containsCounter) {
            visitDepthFirst(
                    *clause, [&](const AstAtom& atom) { relationsToIgnore.insert(atom.getQualifiedName()); });
        }
    }

    return relationsToIgnore;
}

bool AdornDatabaseTransformer::transform(AstTranslationUnit& translationUnit) {
    auto& program = *translationUnit.getProgram();
    auto* ioTypes = translationUnit.getAnalysis<IOType>();

    // Get relations to ignore
    auto relationsToIgnore = getIgnoredRelations(translationUnit);

    // Adorned predicate structure
    using adorned_predicate = std::pair<AstQualifiedName, std::string>;
    auto getAdornmentID = [&](const adorned_predicate& pred) {
        if (pred.second == "") return pred.first;
        AstQualifiedName adornmentID(pred.first);
        std::stringstream adornmentMarker;
        adornmentMarker << "{" << pred.second << "}";
        adornmentID.append(adornmentMarker.str());
        return adornmentID;
    };

    // Process data-structures
    std::vector<std::unique_ptr<AstClause>> adornedClauses;
    std::vector<std::unique_ptr<AstClause>> redundantClauses;
    std::vector<std::unique_ptr<AstRelation>> relationsToAdd;

    std::set<adorned_predicate> headAdornmentsToDo;
    std::set<AstQualifiedName> headAdornmentsSeen;
    std::set<AstQualifiedName> outputRelations;

    // Output relations trigger the adornment process
    for (const auto* rel : program.getRelations()) {
        if (ioTypes->isOutput(rel) || ioTypes->isPrintSize(rel)) {
            auto adornment = std::make_pair(rel->getQualifiedName(), "");
            auto adornmentID = getAdornmentID(adornment);
            assert(!contains(headAdornmentsSeen, adornmentID) && "unexpected repeat output relation");
            headAdornmentsToDo.insert(adornment);
            headAdornmentsSeen.insert(adornmentID);
            outputRelations.insert(rel->getQualifiedName());
        } else if (contains(relationsToIgnore, rel->getQualifiedName())) {
            auto adornment = std::make_pair(rel->getQualifiedName(), "");
            auto adornmentID = getAdornmentID(adornment);
            headAdornmentsToDo.insert(adornment);
            headAdornmentsSeen.insert(adornmentID);
        }
    }

    // Keep going while there's things to adorn
    while (!headAdornmentsToDo.empty()) {
        // Pop off the next head adornment to do
        auto headAdornment = *(headAdornmentsToDo.begin());
        headAdornmentsToDo.erase(headAdornmentsToDo.begin());
        const auto& relName = headAdornment.first;
        const auto* rel = getRelation(program, relName);
        assert(rel != nullptr && "relation does not exist");
        const auto& adornmentMarker = headAdornment.second;

        // Add the adorned relation if needed
        if (adornmentMarker != "") {
            auto adornedRelation = std::make_unique<AstRelation>(getAdornmentID(headAdornment));
            for (const auto* attr : rel->getAttributes()) {
                adornedRelation->addAttribute(std::unique_ptr<AstAttribute>(attr->clone()));
            }
            program.addRelation(std::move(adornedRelation));
        }

        // Adorn every clause correspondingly
        for (const AstClause* clause : getClauses(program, relName)) {
            const auto* headAtom = clause->getHead();
            const auto& headArguments = headAtom->getArguments();
            BindingStore variableBindings(clause);

            // Create the adorned clause with an empty body
            auto adornedClause = std::make_unique<AstClause>();
            auto adornedHeadAtomName = adornmentMarker == "" ? relName : getAdornmentID(headAdornment);
            if (adornmentMarker == "") {
                redundantClauses.push_back(std::unique_ptr<AstClause>(clause->clone()));
            }
            auto adornedHeadAtom = std::make_unique<AstAtom>(adornedHeadAtomName);
            assert((adornmentMarker == "" || headAtom->getArity() == adornmentMarker.length()) &&
                    "adornment marker should correspond to head atom variables");
            for (size_t i = 0; i < adornmentMarker.length(); i++) {
                const auto* var = dynamic_cast<AstVariable*>(headArguments[i]);
                assert(var != nullptr && "expected only variables in head");
                if (adornmentMarker[i] == 'b') {
                    variableBindings.bindHeadVariable(var->getName());
                }
            }

            for (const auto* arg : headArguments) {
                const auto* var = dynamic_cast<const AstVariable*>(arg);
                assert(var != nullptr && "expected only variables in head");
                adornedHeadAtom->addArgument(std::unique_ptr<AstArgument>(var->clone()));
            }

            adornedClause->setHead(std::move(adornedHeadAtom));

            // Check through for variables bound in the body
            visitDepthFirst(*clause, [&](const AstBinaryConstraint& constr) {
                if (constr.getOperator() == BinaryConstraintOp::EQ &&
                        dynamic_cast<AstVariable*>(constr.getLHS()) &&
                        dynamic_cast<AstConstant*>(constr.getRHS())) {
                    const auto* var = dynamic_cast<AstVariable*>(constr.getLHS());
                    variableBindings.bindVariable(var->getName());
                }
            });

            // Add in adorned body literals
            std::vector<std::unique_ptr<AstLiteral>> adornedBodyLiterals;
            for (const auto* lit : clause->getBodyLiterals()) {
                if (const auto* atom = dynamic_cast<const AstAtom*>(lit)) {
                    // Form the appropriate adornment marker
                    std::stringstream atomAdornment;

                    if (!contains(relationsToIgnore, atom->getQualifiedName())) {
                        for (const auto* arg : atom->getArguments()) {
                            const auto* var = dynamic_cast<const AstVariable*>(arg);
                            assert(var != nullptr && "expected only variables in atom");
                            atomAdornment << (variableBindings.isBound(var->getName()) ? "b" : "f");
                        }
                    }

                    auto currAtomAdornment = std::make_pair(atom->getQualifiedName(), atomAdornment.str());
                    auto currAtomAdornmentID = getAdornmentID(currAtomAdornment);

                    // Add the adorned version to the clause
                    auto adornedBodyAtom = std::unique_ptr<AstAtom>(atom->clone());
                    adornedBodyAtom->setQualifiedName(currAtomAdornmentID);
                    adornedBodyLiterals.push_back(std::move(adornedBodyAtom));

                    // Add to the ToDo queue if needed
                    if (!contains(headAdornmentsSeen, currAtomAdornmentID)) {
                        headAdornmentsSeen.insert(currAtomAdornmentID);
                        headAdornmentsToDo.insert(currAtomAdornment);
                    }

                    // All arguments are now bound
                    for (const auto* arg : atom->getArguments()) {
                        const auto* var = dynamic_cast<const AstVariable*>(arg);
                        assert(var != nullptr && "expected only variables in atom");
                        variableBindings.bindVariable(var->getName());
                    }
                } else {
                    adornedBodyLiterals.push_back(std::unique_ptr<AstLiteral>(lit->clone()));
                }
            }
            adornedClause->setBodyLiterals(std::move(adornedBodyLiterals));

            // Add in plans if needed
            if (clause->getExecutionPlan() != nullptr) {
                assert(contains(relationsToIgnore, clause->getHead()->getQualifiedName()) &&
                        "clauses with plans should be ignored");
                adornedClause->setExecutionPlan(
                        std::unique_ptr<AstExecutionPlan>(clause->getExecutionPlan()->clone()));
            }

            adornedClauses.push_back(std::move(adornedClause));
        }
    }

    // Swap over the redundant clauses with the adorned clauses
    for (const auto& clause : redundantClauses) {
        program.removeClause(clause.get());
    }

    for (auto& clause : adornedClauses) {
        program.addClause(std::unique_ptr<AstClause>(clause->clone()));
    }

    return !adornedClauses.empty() || !redundantClauses.empty();
}

AstQualifiedName getNegativeLabel(const AstQualifiedName& name) {
    AstQualifiedName newName(name);
    newName.prepend("@neglabel");
    return newName;
}

bool LabelDatabaseTransformer::transform(AstTranslationUnit& translationUnit) {
    bool changed = false;
    changed |= runNegativeLabelling(translationUnit);
    if (changed) translationUnit.invalidateAnalyses();
    changed |= runPositiveLabelling(translationUnit);
    return changed;
}

bool LabelDatabaseTransformer::runNegativeLabelling(AstTranslationUnit& translationUnit) {
    const auto& sccGraph = *translationUnit.getAnalysis<SCCGraph>();
    const auto& ioTypes = *translationUnit.getAnalysis<IOType>();
    auto& program = *translationUnit.getProgram();

    std::set<AstQualifiedName> relationsToLabel;
    std::set<AstQualifiedName> inputRelations;
    std::set<AstClause*> clausesToAdd;

    for (auto* rel : program.getRelations()) {
        for (const auto* clause : getClauses(program, *rel)) {
            visitDepthFirst(*clause,
                    [&](const AstCounter& /* counter */) { inputRelations.insert(rel->getQualifiedName()); });
        }
        if (ioTypes.isInput(rel)) {
            inputRelations.insert(rel->getQualifiedName());
        }
    }

    // Rename appearances of negated predicates
    visitDepthFirst(program, [&](const AstNegation& neg) {
        auto* atom = neg.getAtom();
        auto relName = atom->getQualifiedName();
        if (contains(inputRelations, relName)) return;
        atom->setQualifiedName(getNegativeLabel(relName));
        relationsToLabel.insert(relName);
    });
    visitDepthFirst(program, [&](const AstAggregator& aggr) {
        visitDepthFirst(aggr, [&](const AstAtom& atom) {
            auto relName = atom.getQualifiedName();
            if (contains(inputRelations, relName)) return;
            const_cast<AstAtom&>(atom).setQualifiedName(getNegativeLabel(relName));
            relationsToLabel.insert(relName);
        });
    });

    // Add the rules for negatively-labelled predicates

    /* Atom labeller */
    struct labelAtoms : public AstNodeMapper {
        const std::set<AstQualifiedName>& sccFriends;
        std::set<AstQualifiedName>& relsToLabel;
        labelAtoms(const std::set<AstQualifiedName>& sccFriends, std::set<AstQualifiedName>& relsToLabel)
                : sccFriends(sccFriends), relsToLabel(relsToLabel) {}
        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            node->apply(*this);
            if (auto* atom = dynamic_cast<AstAtom*>(node.get())) {
                if (contains(sccFriends, atom->getQualifiedName())) {
                    auto labelledAtom = std::unique_ptr<AstAtom>(atom->clone());
                    labelledAtom->setQualifiedName(getNegativeLabel(atom->getQualifiedName()));
                    relsToLabel.insert(atom->getQualifiedName());
                    return labelledAtom;
                }
            }
            return node;
        }
    };

    // Copy over the rules for negatively-labelled relations one stratum at a time
    for (size_t stratum = 0; stratum < sccGraph.getNumberOfSCCs(); stratum++) {
        const auto& rels = sccGraph.getInternalRelations(stratum);
        std::set<AstQualifiedName> relNames;
        for (const auto* rel : rels) {
            relNames.insert(rel->getQualifiedName());
        }

        for (const auto* rel : rels) {
            const auto& relName = rel->getQualifiedName();
            for (auto* clause : getClauses(program, relName)) {
                auto* neggedClause = clause->clone();
                labelAtoms update(relNames, relationsToLabel);
                neggedClause->apply(update);
                clausesToAdd.insert(neggedClause);
            }
        }
    }

    // Add in all the relations that were labelled
    for (const auto& relName : relationsToLabel) {
        const auto* originalRel = getRelation(program, relName);
        assert(originalRel != nullptr && "unlabelled relation does not exist");
        auto labelledRelation = std::unique_ptr<AstRelation>(originalRel->clone());
        labelledRelation->setQualifiedName(getNegativeLabel(relName));
        program.addRelation(std::move(labelledRelation));
    }

    // Add in all the negged clauses
    for (auto* clause : clausesToAdd) {
        program.addClause(std::unique_ptr<AstClause>(clause));
    }

    return !relationsToLabel.empty();
}

bool LabelDatabaseTransformer::runPositiveLabelling(AstTranslationUnit& translationUnit) {
    bool changed = false;

    std::set<AstClause*> clausesToAdd;

    auto& program = *translationUnit.getProgram();
    const auto& sccGraph = *translationUnit.getAnalysis<SCCGraph>();
    const auto& precedenceGraph = translationUnit.getAnalysis<PrecedenceGraph>()->graph();
    const auto& ioTypes = *translationUnit.getAnalysis<IOType>();

    auto isNegativelyLabelled = [&](const AstQualifiedName& name) {
        auto qualifiers = name.getQualifiers();
        assert(!qualifiers.empty() && "unexpected empty qualifier list");
        return qualifiers[0] == "@neglabel";
    };

    /* Atom labeller */
    struct labelAtoms : public AstNodeMapper {
        const AstProgram& program;
        const SCCGraph& sccGraph;
        const std::map<size_t, size_t>& stratumCounts;
        const std::set<AstQualifiedName>& atomsToRelabel;
        labelAtoms(const AstProgram& program, const SCCGraph& sccGraph,
                const std::map<size_t, size_t>& stratumCounts,
                const std::set<AstQualifiedName>& atomsToRelabel)
                : program(program), sccGraph(sccGraph), stratumCounts(stratumCounts),
                  atomsToRelabel(atomsToRelabel) {}

        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            node->apply(*this);
            if (auto* atom = dynamic_cast<AstAtom*>(node.get())) {
                auto relName = atom->getQualifiedName();
                if (contains(atomsToRelabel, relName)) {
                    size_t relStratum = sccGraph.getSCC(getRelation(program, relName));
                    auto relabelledAtom = std::unique_ptr<AstAtom>(atom->clone());
                    auto newName = AstQualifiedName(relName);
                    std::stringstream label;
                    label << "@poscopy_" << stratumCounts.at(relStratum) + 1;
                    newName.prepend(label.str());
                    relabelledAtom->setQualifiedName(newName);
                    return relabelledAtom;
                }
            }
            return node;
        }
    };

    std::set<AstQualifiedName> inputRelations;
    for (auto* rel : program.getRelations()) {
        for (const auto* clause : getClauses(program, *rel)) {
            visitDepthFirst(*clause,
                    [&](const AstCounter& /* counter */) { inputRelations.insert(rel->getQualifiedName()); });
        }
        if (ioTypes.isInput(rel)) {
            inputRelations.insert(rel->getQualifiedName());
        }
    }

    std::set<size_t> labelledStrata;
    std::map<size_t, size_t> labelledStrataCopyCount;
    std::map<size_t, std::set<size_t>> dependentStrata;
    for (size_t stratum = 0; stratum < sccGraph.getNumberOfSCCs(); stratum++) {
        dependentStrata[stratum] = std::set<size_t>();
        size_t neglabelCount = 0;
        const auto& stratumRels = sccGraph.getInternalRelations(stratum);
        for (const auto* rel : stratumRels) {
            if (isNegativelyLabelled(rel->getQualifiedName())) {
                neglabelCount++;
            }
        }
        assert((neglabelCount == 0 || neglabelCount == stratumRels.size()) &&
                "stratum cannot contain a mix of neglabelled and unlabelled relations");
        if (neglabelCount > 0) {
            labelledStrata.insert(stratum);
        } else {
            labelledStrataCopyCount[stratum] = 0;
        }
    }
    for (const auto* rel : program.getRelations()) {
        size_t stratum = sccGraph.getSCC(rel);
        precedenceGraph.visitDepthFirst(rel, [&](const auto* dependentRel) {
            dependentStrata[stratum].insert(sccGraph.getSCC(dependentRel));
        });
    }

    for (size_t stratum = 0; stratum < sccGraph.getNumberOfSCCs(); stratum++) {
        if (!contains(labelledStrata, stratum)) continue;

        const auto& stratumRels = sccGraph.getInternalRelations(stratum);

        // Number the positive derived literals in the associated clauses
        for (const auto* rel : stratumRels) {
            assert(isNegativelyLabelled(rel->getQualifiedName()) &&
                    "should only be looking at neglabelled strata");
            const auto& clauses = getClauses(program, *rel);
            std::set<AstQualifiedName> relsToCopy;
            for (const auto* clause : clauses) {
                visitDepthFirst(*clause, [&](const AstAtom& atom) {
                    const auto& name = atom.getQualifiedName();
                    if (!contains(inputRelations, name) && !isNegativelyLabelled(name)) {
                        relsToCopy.insert(name);
                    }
                });
            }
            for (auto* clause : clauses) {
                labelAtoms update(program, sccGraph, labelledStrataCopyCount, relsToCopy);
                clause->apply(update);
            }
        }

        // Create the rules for the newly positive labelled literals
        std::set<AstQualifiedName> relsToCopy;
        for (const auto* rel : program.getRelations()) {
            const auto& relName = rel->getQualifiedName();
            if (!contains(inputRelations, relName) && !isNegativelyLabelled(relName)) {
                relsToCopy.insert(relName);
            }
        }

        for (int preStratum = stratum - 1; preStratum >= 0; preStratum--) {
            if (contains(labelledStrata, preStratum)) continue;
            if (contains(dependentStrata[preStratum], stratum)) {
                const auto& preStratumRels = sccGraph.getInternalRelations(preStratum);
                std::set<AstQualifiedName> relsToLabel;
                for (const auto* rel : preStratumRels) {
                    relsToLabel.insert(rel->getQualifiedName());
                }
                for (const auto* rel : preStratumRels) {
                    if (contains(inputRelations, rel->getQualifiedName())) continue;
                    for (const auto* clause : getClauses(program, rel->getQualifiedName())) {
                        auto* labelledClause = clause->clone();
                        labelAtoms update(program, sccGraph, labelledStrataCopyCount, relsToCopy);
                        labelledClause->apply(update);
                        program.addClause(std::unique_ptr<AstClause>(labelledClause));
                    }
                }
                labelledStrataCopyCount[preStratum]++;
            }
        }
    }

    // Add the new relations in
    for (auto& pair : labelledStrataCopyCount) {
        size_t stratum = pair.first;
        const auto& stratumRels = sccGraph.getInternalRelations(stratum);
        for (size_t copy = 0; copy < pair.second; copy++) {
            for (auto* rel : stratumRels) {
                std::stringstream label;
                label << "@poscopy_" << copy + 1;
                auto newName = AstQualifiedName(rel->getQualifiedName());
                newName.prepend(label.str());
                auto* newRelation = rel->clone();
                newRelation->setQualifiedName(newName);
                program.addRelation(std::unique_ptr<AstRelation>(newRelation));
            }
        }
    }

    return changed;
}

bool MagicSetTransformer::transform(AstTranslationUnit& translationUnit) {
    auto& program = *translationUnit.getProgram();
    std::set<std::unique_ptr<AstClause>> clausesToRemove;
    std::set<std::unique_ptr<AstClause>> clausesToAdd;

    std::set<AstQualifiedName> magicPredicatesSeen;

    /** Checks if a given relation name is adorned */
    auto isAdorned = [&](const AstQualifiedName& name) {
        auto qualifiers = name.getQualifiers();
        assert(!qualifiers.empty() && "unexpected empty qualifier list");
        auto finalQualifier = qualifiers[qualifiers.size() - 1];
        assert(finalQualifier.length() > 0 && "unexpected empty qualifier");
        if (finalQualifier[0] == '{') {
            assert(finalQualifier[finalQualifier.length() - 1] == '}' && "unterminated adornment string");
            for (size_t i = 1; i < finalQualifier.length() - 1; i++) {
                char curBindingType = finalQualifier[i];
                assert((curBindingType == 'b' || curBindingType == 'f') &&
                        "unexpected binding type in adornment");
            }
            return true;
        }
        return false;
    };

    /** Retrieves the adornment encoded in a given relation name */
    auto getAdornment = [&](const AstQualifiedName& name) {
        assert(isAdorned(name) && "relation not adorned");
        auto qualifiers = name.getQualifiers();
        auto finalQualifier = qualifiers[qualifiers.size() - 1];
        std::stringstream binding;
        for (size_t i = 1; i < finalQualifier.length() - 1; i++) {
            binding << finalQualifier[i];
        }
        return binding.str();
    };

    /** Create the magic atom associated with the given (relation, adornment) pair */
    auto createMagicAtom = [&](const AstAtom* atom) {
        auto name = atom->getQualifiedName();
        auto magicRelName = AstQualifiedName(name);
        magicRelName.prepend("@magic");

        auto args = atom->getArguments();
        auto adornmentMarker = getAdornment(name);
        auto magicAtom = std::make_unique<AstAtom>(magicRelName);
        for (size_t i = 0; i < args.size(); i++) {
            if (adornmentMarker[i] == 'b') {
                magicAtom->addArgument(std::unique_ptr<AstArgument>(args[i]->clone()));
            }
        }

        if (!contains(magicPredicatesSeen, magicRelName)) {
            magicPredicatesSeen.insert(magicRelName);

            auto attributes = getRelation(program, name)->getAttributes();
            auto magicRelation = std::make_unique<AstRelation>(magicRelName);
            for (size_t i = 0; i < attributes.size(); i++) {
                if (adornmentMarker[i] == 'b') {
                    magicRelation->addAttribute(std::unique_ptr<AstAttribute>(attributes[i]->clone()));
                }
            }
            program.addRelation(std::move(magicRelation));
        }

        return magicAtom;
    };

    /** Create magic clause focused on a specific atom */
    auto createMagicClause = [&](const AstAtom* atom,
                                     const std::vector<std::unique_ptr<AstAtom>>& constrainingAtoms,
                                     const std::vector<const AstBinaryConstraint*> eqConstraints) {
        auto magicHead = createMagicAtom(atom);
        auto magicClause = std::make_unique<AstClause>();
        for (const auto& bindingAtom : constrainingAtoms) {
            magicClause->addToBody(std::unique_ptr<AstAtom>(bindingAtom->clone()));
        }

        std::set<std::string> seenVariables;
        visitDepthFirst(
                constrainingAtoms, [&](const AstVariable& var) { seenVariables.insert(var.getName()); });
        visitDepthFirst(*magicHead, [&](const AstVariable& var) { seenVariables.insert(var.getName()); });
        bool fixpointReached = false;
        while (!fixpointReached) {
            fixpointReached = true;
            for (const auto* eqConstraint : eqConstraints) {
                if (dynamic_cast<AstRecordInit*>(eqConstraint->getRHS()) != nullptr) {
                    const auto* var = dynamic_cast<const AstVariable*>(eqConstraint->getLHS());
                    if (var != nullptr && contains(seenVariables, var->getName())) {
                        visitDepthFirst(*eqConstraint, [&](const AstVariable& subVar) {
                            if (!contains(seenVariables, subVar.getName())) {
                                fixpointReached = false;
                                seenVariables.insert(subVar.getName());
                            }
                        });
                    }
                }
                if (dynamic_cast<AstRecordInit*>(eqConstraint->getLHS()) != nullptr) {
                    const auto* var = dynamic_cast<const AstVariable*>(eqConstraint->getRHS());
                    if (var != nullptr && contains(seenVariables, var->getName())) {
                        visitDepthFirst(*eqConstraint, [&](const AstVariable& subVar) {
                            if (!contains(seenVariables, subVar.getName())) {
                                fixpointReached = false;
                                seenVariables.insert(subVar.getName());
                            }
                        });
                    }
                }
            }
        }

        for (const auto* eqConstraint : eqConstraints) {
            bool addConstraint = true;
            visitDepthFirst(*eqConstraint, [&](const AstVariable& var) {
                if (!contains(seenVariables, var.getName())) {
                    addConstraint = false;
                }
            });

            if (addConstraint) {
                magicClause->addToBody(std::unique_ptr<AstBinaryConstraint>(eqConstraint->clone()));
            }
        }

        magicClause->setHead(std::move(magicHead));
        return magicClause;
    };

    /** Get all equality constraints in a clause */
    auto getEqualityConstraints = [&](const AstClause* clause) {
        std::vector<const AstBinaryConstraint*> equalityConstraints;
        for (const auto* lit : clause->getBodyLiterals()) {
            const auto* bc = dynamic_cast<const AstBinaryConstraint*>(lit);
            if (bc == nullptr || bc->getOperator() != BinaryConstraintOp::EQ) continue;
            if (dynamic_cast<AstVariable*>(bc->getLHS()) != nullptr ||
                    dynamic_cast<AstConstant*>(bc->getRHS()) != nullptr) {
                bool containsAggrs = false;
                visitDepthFirst(*bc, [&](const AstAggregator& /* aggr */) { containsAggrs = true; });
                if (!containsAggrs) {
                    equalityConstraints.push_back(bc);
                }
            }
        }
        return equalityConstraints;
    };

    /** Perform the Magic Set Transformation */
    for (const auto* clause : program.getClauses()) {
        clausesToRemove.insert(std::unique_ptr<AstClause>(clause->clone()));

        const auto* head = clause->getHead();
        auto relName = head->getQualifiedName();

        // (1) Add the refined clause
        if (!isAdorned(relName)) {
            // Unadorned relations need not be refined, as every possible tuple is relevant
            clausesToAdd.insert(std::unique_ptr<AstClause>(clause->clone()));
        } else {
            // Refine the clause with a prepended magic atom
            auto magicAtom = createMagicAtom(head);
            auto refinedClause = std::make_unique<AstClause>();
            refinedClause->setHead(std::unique_ptr<AstAtom>(head->clone()));
            refinedClause->addToBody(std::unique_ptr<AstAtom>(magicAtom->clone()));
            for (auto* literal : clause->getBodyLiterals()) {
                refinedClause->addToBody(std::unique_ptr<AstLiteral>(literal->clone()));
            }
            clausesToAdd.insert(std::move(refinedClause));
        }

        // (2) Add the associated magic rules
        std::vector<const AstBinaryConstraint*> eqConstraints = getEqualityConstraints(clause);
        std::vector<std::unique_ptr<AstAtom>> atomsToTheLeft;
        if (isAdorned(relName)) {
            // Add the specialising head atom
            // Output relations are not specialised, and so the head will not contribute to specialisation
            atomsToTheLeft.push_back(createMagicAtom(clause->getHead()));
        }
        for (const auto* lit : clause->getBodyLiterals()) {
            const auto* atom = dynamic_cast<const AstAtom*>(lit);
            if (atom == nullptr) continue;
            if (!isAdorned(atom->getQualifiedName())) {
                atomsToTheLeft.push_back(std::unique_ptr<AstAtom>(atom->clone()));
                continue;
            }
            auto magicClause = createMagicClause(atom, atomsToTheLeft, eqConstraints);
            atomsToTheLeft.push_back(std::unique_ptr<AstAtom>(atom->clone()));
            clausesToAdd.insert(std::move(magicClause));
        }
    }

    for (auto& clause : clausesToAdd) {
        program.addClause(std::unique_ptr<AstClause>(clause->clone()));
    }
    for (const auto& clause : clausesToRemove) {
        program.removeClause(clause.get());
    }

    return !clausesToRemove.empty() || !clausesToAdd.empty();
}

}  // namespace souffle
