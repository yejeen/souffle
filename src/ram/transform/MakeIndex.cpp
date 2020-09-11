/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file MakeIndex.cpp
 *
 ***********************************************************************/

#include "ram/transform/MakeIndex.h"
#include "FunctorOps.h"
#include "RelationTag.h"
#include "ram/Condition.h"
#include "ram/Constraint.h"
#include "ram/Expression.h"
#include "ram/Node.h"
#include "ram/Operation.h"
#include "ram/Program.h"
#include "ram/Relation.h"
#include "ram/Statement.h"
#include "ram/Utils.h"
#include "ram/Visitor.h"
#include "souffle/BinaryConstraintOps.h"
#include "souffle/RamTypes.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include <algorithm>
#include <cstddef>
#include <functional>
#include <memory>
#include <tuple>
#include <utility>
#include <vector>

namespace souffle {

using ExpressionPair = std::pair<Own<RamExpression>, Own<RamExpression>>;

ExpressionPair MakeIndexTransformer::getExpressionPair(
        const RamConstraint* binRelOp, size_t& element, int identifier) {
    if (isLessEqual(binRelOp->getOperator())) {
        // Tuple[level, element] <= <expr>
        if (const auto* lhs = dynamic_cast<const RamTupleElement*>(&binRelOp->getLHS())) {
            const RamExpression* rhs = &binRelOp->getRHS();
            if (lhs->getTupleId() == identifier && rla->getLevel(rhs) < identifier) {
                element = lhs->getElement();
                return {mk<RamUndefValue>(), clone(rhs)};
            }
        }
        // <expr> <= Tuple[level, element]
        if (const auto* rhs = dynamic_cast<const RamTupleElement*>(&binRelOp->getRHS())) {
            const RamExpression* lhs = &binRelOp->getLHS();
            if (rhs->getTupleId() == identifier && rla->getLevel(lhs) < identifier) {
                element = rhs->getElement();
                return {clone(lhs), mk<RamUndefValue>()};
            }
        }
    }

    if (isGreaterEqual(binRelOp->getOperator())) {
        // Tuple[level, element] >= <expr>
        if (const auto* lhs = dynamic_cast<const RamTupleElement*>(&binRelOp->getLHS())) {
            const RamExpression* rhs = &binRelOp->getRHS();
            if (lhs->getTupleId() == identifier && rla->getLevel(rhs) < identifier) {
                element = lhs->getElement();
                return {clone(rhs), mk<RamUndefValue>()};
            }
        }
        // <expr> >= Tuple[level, element]
        if (const auto* rhs = dynamic_cast<const RamTupleElement*>(&binRelOp->getRHS())) {
            const RamExpression* lhs = &binRelOp->getLHS();
            if (rhs->getTupleId() == identifier && rla->getLevel(lhs) < identifier) {
                element = rhs->getElement();
                return {mk<RamUndefValue>(), clone(lhs)};
            }
        }
    }
    return {mk<RamUndefValue>(), mk<RamUndefValue>()};
}

// Retrieves the <expr1> <= Tuple[level, element] <= <expr2> part of the constraint as a pair { <expr1>,
// <expr2> }
ExpressionPair MakeIndexTransformer::getLowerUpperExpression(
        RamCondition* c, size_t& element, int identifier) {
    if (auto* binRelOp = dynamic_cast<RamConstraint*>(c)) {
        if (isEqConstraint(binRelOp->getOperator())) {
            if (const auto* lhs = dynamic_cast<const RamTupleElement*>(&binRelOp->getLHS())) {
                const RamExpression* rhs = &binRelOp->getRHS();
                if (lhs->getTupleId() == identifier && rla->getLevel(rhs) < identifier) {
                    element = lhs->getElement();
                    return {clone(rhs), clone(rhs)};
                }
            }
            if (const auto* rhs = dynamic_cast<const RamTupleElement*>(&binRelOp->getRHS())) {
                const RamExpression* lhs = &binRelOp->getLHS();
                if (rhs->getTupleId() == identifier && rla->getLevel(lhs) < identifier) {
                    element = rhs->getElement();
                    return {clone(lhs), clone(lhs)};
                }
            }
        } else if (isWeakIneqConstraint(binRelOp->getOperator())) {
            return getExpressionPair(binRelOp, element, identifier);
        }
    }
    return {mk<RamUndefValue>(), mk<RamUndefValue>()};
}

Own<RamCondition> MakeIndexTransformer::constructPattern(const std::vector<std::string>& attributeTypes,
        RamPattern& queryPattern, bool& indexable, VecOwn<RamCondition> conditionList, int identifier) {
    // Remaining conditions which cannot be handled by an index
    Own<RamCondition> condition;
    auto addCondition = [&](Own<RamCondition> c) {
        if (condition != nullptr) {
            condition = mk<RamConjunction>(std::move(condition), std::move(c));
        } else {
            condition = std::move(c);
        }
    };

    // transform condition list so that every strict inequality becomes a weak inequality + filter
    // e.g. Tuple[level, element] < <expr> --> Tuple[level, element] <= <expr> and Tuple[level, element] !=
    // <expr>
    std::vector<std::unique_ptr<RamCondition>> toAppend;
    auto it = conditionList.begin();
    while (it != conditionList.end()) {
        auto& cond = *it;
        if (auto* binRelOp = dynamic_cast<RamConstraint*>(cond.get())) {
            bool transformable = false;
            size_t element = 0;

            if (isStrictIneqConstraint(binRelOp->getOperator())) {
                if (const auto* lhs = dynamic_cast<const RamTupleElement*>(&binRelOp->getLHS())) {
                    const RamExpression* rhs = &binRelOp->getRHS();
                    if (lhs->getTupleId() == identifier && rla->getLevel(rhs) < identifier) {
                        transformable = true;
                        element = lhs->getElement();
                    }
                }
                if (const auto* rhs = dynamic_cast<const RamTupleElement*>(&binRelOp->getRHS())) {
                    const RamExpression* lhs = &binRelOp->getLHS();
                    if (rhs->getTupleId() == identifier && rla->getLevel(lhs) < identifier) {
                        transformable = true;
                        element = rhs->getElement();
                    }
                }
            }

            bool interpreted = !Global::config().has("compile") && !Global::config().has("dl-program") &&
                               !Global::config().has("generate") && !Global::config().has("swig");

            if (transformable) {
                // if (!interpreted || attributeTypes[element][0] == 'i') {
                // append the weak version of inequality
                toAppend.emplace_back(std::make_unique<RamConstraint>(
                        convertStrictToWeakIneqConstraint(binRelOp->getOperator()),
                        clone(&binRelOp->getLHS()), clone(&binRelOp->getRHS())));
                // append the != constraint
                toAppend.emplace_back(std::make_unique<RamConstraint>(
                        convertStrictToNotEqualConstraint(binRelOp->getOperator()),
                        clone(&binRelOp->getLHS()), clone(&binRelOp->getRHS())));

                // remove the strict version of inequality
                it = conditionList.erase(it);
                continue;
                //}
            }
        }
        ++it;
    }

    std::transform(toAppend.begin(), toAppend.end(), std::back_inserter(conditionList),
            [](const std::unique_ptr<RamCondition>& cond) { return std::move(clone(cond)); });

    // Build query pattern and remaining condition
    for (auto& cond : conditionList) {
        size_t element = 0;
        Own<RamExpression> lowerExpression;
        Own<RamExpression> upperExpression;
        std::tie(lowerExpression, upperExpression) = getLowerUpperExpression(cond.get(), element, identifier);

        // we have new bounds if at least one is defined
        if (!isRamUndefValue(lowerExpression.get()) || !isRamUndefValue(upperExpression.get())) {
            // if no previous bounds are set then just assign them, consider both bounds to be set (but not
            // necessarily defined) in all remaining cases
            auto type = attributeTypes[element];
            indexable = true;
            if (isRamUndefValue(queryPattern.first[element].get()) &&
                    isRamUndefValue(queryPattern.second[element].get())) {
                queryPattern.first[element] = std::move(lowerExpression);
                queryPattern.second[element] = std::move(upperExpression);
                // if lower bound is undefined and we have a new lower bound then assign it
            } else if (isRamUndefValue(queryPattern.first[element].get()) &&
                       !isRamUndefValue(lowerExpression.get()) && isRamUndefValue(upperExpression.get())) {
                queryPattern.first[element] = std::move(lowerExpression);
                // if upper bound is undefined and we have a new upper bound then assign it
            } else if (isRamUndefValue(queryPattern.second[element].get()) &&
                       isRamUndefValue(lowerExpression.get()) && !isRamUndefValue(upperExpression.get())) {
                queryPattern.second[element] = std::move(upperExpression);
                // if both bounds are defined ...
                // and equal then we have a previous equality constraint i.e. Tuple[level, element] = <expr1>
            } else if (!isRamUndefValue(queryPattern.first[element].get()) &&
                       !isRamUndefValue(queryPattern.second[element].get()) &&
                       (*(queryPattern.first[element]) == *(queryPattern.second[element]))) {
                // new equality constraint i.e. Tuple[level, element] = <expr2>
                // simply hoist <expr1> = <expr2> to the outer loop
                if (!isRamUndefValue(lowerExpression.get()) && !isRamUndefValue(upperExpression.get())) {
                    // FIXME: `FEQ` handling; need to know if the expr is a float exp or not
                    addCondition(mk<RamConstraint>(getEqConstraint(type),
                            souffle::clone(queryPattern.first[element]), std::move(lowerExpression)));
                }
                // new lower bound i.e. Tuple[level, element] >= <expr2>
                // we need to hoist <expr1> >= <expr2> to the outer loop
                else if (!isRamUndefValue(lowerExpression.get()) && isRamUndefValue(upperExpression.get())) {
                    addCondition(mk<RamConstraint>(getGreaterEqualConstraint(type),
                            souffle::clone(queryPattern.first[element]), std::move(lowerExpression)));
                }
                // new upper bound i.e. Tuple[level, element] <= <expr2>
                // we need to hoist <expr1> <= <expr2> to the outer loop
                else if (isRamUndefValue(lowerExpression.get()) && !isRamUndefValue(upperExpression.get())) {
                    addCondition(mk<RamConstraint>(getLessEqualConstraint(type),
                            souffle::clone(queryPattern.first[element]), std::move(upperExpression)));
                }
                // if either bound is defined but they aren't equal we must consider the cases for updating
                // them note that at this point we know that if we have a lower/upper bound it can't be the
                // first one
            } else if (!isRamUndefValue(queryPattern.first[element].get()) ||
                       !isRamUndefValue(queryPattern.second[element].get())) {
                // if we have a new equality constraint and previous inequality constraints
                if (!isRamUndefValue(lowerExpression.get()) && !isRamUndefValue(upperExpression.get()) &&
                        *lowerExpression == *upperExpression) {
                    // if Tuple[level, element] >= <expr1> and we see Tuple[level, element] = <expr2>
                    // need to hoist <expr2> >= <expr1> to the outer loop
                    if (!isRamUndefValue(queryPattern.first[element].get())) {
                        addCondition(mk<RamConstraint>(getGreaterEqualConstraint(type),
                                souffle::clone(lowerExpression), std::move(queryPattern.first[element])));
                    }
                    // if Tuple[level, element] <= <expr1> and we see Tuple[level, element] = <expr2>
                    // need to hoist <expr2> <= <expr1> to the outer loop
                    if (!isRamUndefValue(queryPattern.second[element].get())) {
                        addCondition(mk<RamConstraint>(getLessEqualConstraint(type),
                                souffle::clone(upperExpression), std::move(queryPattern.second[element])));
                    }
                    // finally replace bounds with equality constraint
                    queryPattern.first[element] = std::move(lowerExpression);
                    queryPattern.second[element] = std::move(upperExpression);
                    // if we have a new lower bound
                } else if (!isRamUndefValue(lowerExpression.get())) {
                    // we want the tightest lower bound so we take the max
                    VecOwn<RamExpression> maxArguments;
                    maxArguments.push_back(std::move(queryPattern.first[element]));
                    maxArguments.push_back(std::move(lowerExpression));

                    queryPattern.first[element] =
                            mk<RamIntrinsicOperator>(getMaxOp(type), std::move(maxArguments));
                    // if we have a new upper bound
                } else if (!isRamUndefValue(upperExpression.get())) {
                    // we want the tightest upper bound so we take the min
                    VecOwn<RamExpression> minArguments;
                    minArguments.push_back(std::move(queryPattern.second[element]));
                    minArguments.push_back(std::move(upperExpression));

                    queryPattern.second[element] =
                            mk<RamIntrinsicOperator>(getMinOp(type), std::move(minArguments));
                }
            }
        } else {
            addCondition(std::move(cond));
        }
    }

    // Avoid null-pointers for condition and query pattern
    if (condition == nullptr) {
        condition = mk<RamTrue>();
    }
    return condition;
}

Own<RamOperation> MakeIndexTransformer::rewriteAggregate(const RamAggregate* agg) {
    if (dynamic_cast<const RamTrue*>(&agg->getCondition()) == nullptr) {
        const RamRelation& rel = agg->getRelation();
        int identifier = agg->getTupleId();
        RamPattern queryPattern;
        for (unsigned int i = 0; i < rel.getArity(); ++i) {
            queryPattern.first.push_back(mk<RamUndefValue>());
            queryPattern.second.push_back(mk<RamUndefValue>());
        }

        bool indexable = false;
        Own<RamCondition> condition = constructPattern(rel.getAttributeTypes(), queryPattern, indexable,
                toConjunctionList(&agg->getCondition()), identifier);
        if (indexable) {
            return mk<RamIndexAggregate>(souffle::clone(&agg->getOperation()), agg->getFunction(),
                    mk<RamRelationReference>(&rel), souffle::clone(&agg->getExpression()),
                    std::move(condition), std::move(queryPattern), agg->getTupleId());
        }
    }
    return nullptr;
}

Own<RamOperation> MakeIndexTransformer::rewriteScan(const RamScan* scan) {
    if (const auto* filter = dynamic_cast<const RamFilter*>(&scan->getOperation())) {
        const RamRelation& rel = scan->getRelation();
        const int identifier = scan->getTupleId();
        RamPattern queryPattern;
        for (unsigned int i = 0; i < rel.getArity(); ++i) {
            queryPattern.first.push_back(mk<RamUndefValue>());
            queryPattern.second.push_back(mk<RamUndefValue>());
        }

        bool indexable = false;
        Own<RamCondition> condition = constructPattern(rel.getAttributeTypes(), queryPattern, indexable,
                toConjunctionList(&filter->getCondition()), identifier);
        if (indexable) {
            Own<RamOperation> op = souffle::clone(&filter->getOperation());
            if (!isRamTrue(condition.get())) {
                op = mk<RamFilter>(std::move(condition), std::move(op));
            }
            return mk<RamIndexScan>(mk<RamRelationReference>(&rel), identifier, std::move(queryPattern),
                    std::move(op), scan->getProfileText());
        }
    }
    return nullptr;
}

Own<RamOperation> MakeIndexTransformer::rewriteIndexScan(const RamIndexScan* iscan) {
    if (const auto* filter = dynamic_cast<const RamFilter*>(&iscan->getOperation())) {
        const RamRelation& rel = iscan->getRelation();
        const int identifier = iscan->getTupleId();

        RamPattern strengthenedPattern;
        strengthenedPattern.first = clone(iscan->getRangePattern().first);
        strengthenedPattern.second = clone(iscan->getRangePattern().second);

        bool indexable = false;
        // strengthen the pattern with construct pattern
        Own<RamCondition> condition = constructPattern(rel.getAttributeTypes(), strengthenedPattern,
                indexable, toConjunctionList(&filter->getCondition()), identifier);

        if (indexable) {
            // Merge Index Pattern here

            Own<RamOperation> op = souffle::clone(&filter->getOperation());
            if (!isRamTrue(condition.get())) {
                op = mk<RamFilter>(std::move(condition), std::move(op));
            }
            return mk<RamIndexScan>(mk<RamRelationReference>(&rel), identifier,
                    std::move(strengthenedPattern), std::move(op), iscan->getProfileText());
        }
    }
    return nullptr;
}

bool MakeIndexTransformer::makeIndex(RamProgram& program) {
    bool changed = false;
    visitDepthFirst(program, [&](const RamQuery& query) {
        std::function<Own<RamNode>(Own<RamNode>)> scanRewriter = [&](Own<RamNode> node) -> Own<RamNode> {
            if (const RamScan* scan = dynamic_cast<RamScan*>(node.get())) {
                if (scan->getRelation().getRepresentation() != RelationRepresentation::INFO) {
                    if (Own<RamOperation> op = rewriteScan(scan)) {
                        changed = true;
                        node = std::move(op);
                    }
                }
            } else if (const RamIndexScan* iscan = dynamic_cast<RamIndexScan*>(node.get())) {
                if (Own<RamOperation> op = rewriteIndexScan(iscan)) {
                    changed = true;
                    node = std::move(op);
                }
            } else if (const RamAggregate* agg = dynamic_cast<RamAggregate*>(node.get())) {
                if (Own<RamOperation> op = rewriteAggregate(agg)) {
                    changed = true;
                    node = std::move(op);
                }
            }
            node->apply(makeLambdaRamMapper(scanRewriter));
            return node;
        };
        const_cast<RamQuery*>(&query)->apply(makeLambdaRamMapper(scanRewriter));
    });
    return changed;
}

}  // end of namespace souffle
