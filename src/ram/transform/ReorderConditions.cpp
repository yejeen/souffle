/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ReorderConditions.cpp
 *
 ***********************************************************************/

#include "ram/transform/ReorderConditions.h"
#include "ram/Condition.h"
#include "ram/Node.h"
#include "ram/Operation.h"
#include "ram/Program.h"
#include "ram/Statement.h"
#include "ram/Utils.h"
#include "ram/Visitor.h"
#include "ram/analysis/Complexity.h"
#include "souffle/utility/MiscUtil.h"
#include <algorithm>
#include <functional>
#include <memory>
#include <vector>

namespace souffle {

bool ReorderConditionsTransformer::reorderConditions(RamProgram& program) {
    bool changed = false;
    visitDepthFirst(program, [&](const RamQuery& query) {
        std::function<Own<RamNode>(Own<RamNode>)> filterRewriter = [&](Own<RamNode> node) -> Own<RamNode> {
            if (const RamFilter* filter = dynamic_cast<RamFilter*>(node.get())) {
                const RamCondition* condition = &filter->getCondition();
                VecOwn<RamCondition> sortedConds;
                VecOwn<RamCondition> condList = toConjunctionList(condition);
                for (auto& cond : condList) {
                    sortedConds.emplace_back(cond->clone());
                }
                std::sort(sortedConds.begin(), sortedConds.end(),
                        [&](Own<RamCondition>& a, Own<RamCondition>& b) {
                            return rca->getComplexity(a.get()) < rca->getComplexity(b.get());
                        });

                if (!std::equal(sortedConds.begin(), sortedConds.end(), condList.begin(),
                            [](Own<RamCondition>& a, Own<RamCondition>& b) { return *a == *b; })) {
                    changed = true;
                    node = std::make_unique<RamFilter>(Own<RamCondition>(toCondition(sortedConds)),
                            souffle::clone(&filter->getOperation()));
                }
            }
            node->apply(makeLambdaRamMapper(filterRewriter));
            return node;
        };
        const_cast<RamQuery*>(&query)->apply(makeLambdaRamMapper(filterRewriter));
    });
    return changed;
}

}  // end of namespace souffle
