/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ADTtoRecords.cpp
 *
 ***********************************************************************/

#include "ast/transform/ADTtoRecords.h"
#include "ast/ADTinit.h"
#include "ast/AlgebraicDataType.h"
#include "ast/Argument.h"
#include "ast/Node.h"
#include "ast/NumericConstant.h"
#include "ast/RecordInit.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/TypeEnvironment.h"
#include "ast/analysis/TypeSystem.h"
#include "utility/ContainerUtil.h"
#include <cassert>
#include <memory>

namespace souffle {

bool ADTtoRecords::transform(AstTranslationUnit& tu) {
    struct ADTsFuneral : public AstNodeMapper {
        mutable bool changed{false};
        AstTranslationUnit& tu;
        const SumTypeBranchesAnalysis& sumTypesBranches = *tu.getAnalysis<SumTypeBranchesAnalysis>();

        ADTsFuneral(AstTranslationUnit& tu) : tu(tu){};

        Own<AstNode> operator()(Own<AstNode> node) const override {
            // Rewrite sub-expressions first
            node->apply(*this);

            if (!isA<AstADTinit>(node)) {
                return node;
            }

            changed = true;

            auto& adt = *as<AstADTinit>(node);

            auto& type = sumTypesBranches.unsafeGetType(adt.getBranch());
            assert(isA<SumType>(type));

            auto& branches = as<SumType>(type)->getBranches();

            // Find branch ID.
            SumType::Branch searchDummy = {adt.getBranch(), nullptr};
            auto iterToBranch = std::lower_bound(branches.begin(), branches.end(), searchDummy,
                    [](const SumType::Branch& left, const SumType::Branch& right) {
                        return left.name < right.name;
                    });
            auto branchID = std::distance(std::begin(branches), iterToBranch);

            VecOwn<AstArgument> recordArgs;
            recordArgs.push_back(mk<AstArgument, AstNumericConstant>(branchID));
            recordArgs.emplace_back(adt.getArgument()->clone());

            return mk<AstRecordInit>(std::move(recordArgs), adt.getSrcLoc());
        }
    };

    ADTsFuneral mapper(tu);
    tu.getProgram()->apply(mapper);
    return mapper.changed;
}

}  // namespace souffle
