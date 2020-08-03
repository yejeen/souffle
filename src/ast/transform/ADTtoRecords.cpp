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
#include "ast/Argument.h"
#include "ast/Node.h"
#include "ast/TranslationUnit.h"
#include "ast/TypeSystem.h"
#include "ast/analysis/TypeEnvironment.h"

namespace souffle {

bool ADTtoRecords::transform(AstTranslationUnit& tu) {
    struct ADTsFuneral : public AstNodeMapper {
        bool changed{false};
        const TypeEnvironment& env;

        ADTsFuneral(const TypeEnvironment& env) : env(env){};

        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            if (!isA<AstADTinit>(node)) {
                return node;
            }
            // changed = true;

            // auto& adt = *as<AstADTinit>(node);

            // // Rewrite sub-expressions first
            // node->apply(*this);

            // assert(isA<SumType>(env.getType(r.type)));
            // auto& type = *as<SumType>(env.getType(r.type));
            return node;
        }
    };

    ADTsFuneral mapper(tu.getAnalysis<TypeEnvironmentAnalysis>()->getTypeEnvironment());
    tu.getProgram()->apply(mapper);
    return mapper.changed;
}

}  // namespace souffle
