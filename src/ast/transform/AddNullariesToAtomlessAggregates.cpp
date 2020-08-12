/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AddNullariesToAtomlessAggregates.cpp
 *
 ***********************************************************************/

#include "ast/transform/AddNullariesToAtomlessAggregates.h"
#include "AggregateOp.h"
#include "RamTypes.h"
#include "ast/Aggregator.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/Attribute.h"
#include "ast/Clause.h"
#include "ast/LambdaNodeMapper.h"
#include "ast/Literal.h"
#include "ast/Node.h"
#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/Relation.h"
#include "ast/TranslationUnit.h"
#include "ast/TypeSystem.h"
#include "ast/UnnamedVariable.h"
#include "ast/Utils.h"
#include "ast/Variable.h"
#include "ast/Visitor.h"
#include "ast/analysis/Ground.h"
#include "ast/analysis/Type.h"
#include "ast/analysis/TypeEnvironment.h"
#include "utility/MiscUtil.h"
#include "utility/StringUtil.h"
#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <utility>
#include <vector>

namespace souffle {

bool AddNullariesToAtomlessAggregatesTransformer::transform(AstTranslationUnit& translationUnit) {
    bool changed{false};
    AstProgram& program = *translationUnit.getProgram();
    visitDepthFirst(program, [&](const AstAggregator& agg) {
        bool seenAtom{false};
        for (const auto& literal : agg.getBodyLiterals()) {
            if (dynamic_cast<const AstAtom*>(literal) != nullptr) {
                seenAtom = true;
            }
        }
        if (seenAtom) {
            return;
        }
        // We will add in the Tautology atom to the body of this aggregate now
        changed = true;
        // +Tautology()
        auto nullaryAtom = std::make_unique<AstAtom>();
        std::string relName = "+Tautology";
        nullaryAtom->setQualifiedName(relName);

        if (getRelation(program, relName) == nullptr) {
            // +Tautology().
            auto fact = std::make_unique<AstClause>();
            fact->setHead(souffle::clone(nullaryAtom));
            // .decl +Tautology()
            auto tautologyRel = std::make_unique<AstRelation>();
            tautologyRel->setQualifiedName(relName);
            program.addRelation(std::move(tautologyRel));
            program.addClause(std::move(fact));
        }
        VecOwn<AstLiteral> newBody;
        for (const auto& lit : agg.getBodyLiterals()) {
            newBody.push_back(souffle::clone(lit));
        }
        newBody.push_back(souffle::clone(nullaryAtom));
        const_cast<AstAggregator&>(agg).setBody(std::move(newBody));
    });
    return changed;
}
}  // end of namespace souffle
