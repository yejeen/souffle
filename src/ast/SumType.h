/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All Rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SumType.h
 *
 * Defines the node corresponding to an ast declaration of Algebraic Data Type
 *
 ***********************************************************************/

#pragma once

#include "ast/BranchDeclaration.h"
#include "ast/Type.h"
#include "parser/SrcLocation.h"
#include "utility/tinyformat.h"

namespace souffle {

/**
 * Sum types aka tagged union
 */
class AstSumType : public AstType {
public:
    AstSumType(AstQualifiedName name, VecOwn<AstBranchDeclaration> bs, SrcLocation loc = {})
            : AstType(std::move(name), std::move(loc)), branches(std::move(bs)) {
        assert(!branches.empty());
    };

    std::vector<AstBranchDeclaration*> getBranches() const {
        return toPtrVector(branches);
    }

    void print(std::ostream& os) const override {
        os << tfm::format(".type %s = %s", getQualifiedName(), join(branches, " ; "));
    }

    AstSumType* clone() const override {
        return new AstSumType(getQualifiedName(), souffle::clone(branches), getSrcLoc());
    }

protected:
    bool equal(const AstNode& node) const override {
        const auto& other = dynamic_cast<const AstSumType&>(node);
        return getQualifiedName() == other.getQualifiedName() && branches == other.branches;
    }

private:
    /** The list of branches for this sum type. */
    VecOwn<AstBranchDeclaration> branches;
};

}  // namespace souffle
