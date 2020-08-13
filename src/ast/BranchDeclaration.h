/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All Rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file BranchDeclaration.h
 *
 * Defines the wrapper for a single branch in ADT declaration.
 *
 ***********************************************************************/

#pragma once

#include "ast/Attribute.h"
#include "ast/Node.h"
#include "ast/QualifiedName.h"
#include "parser/SrcLocation.h"
#include "utility/ContainerUtil.h"
#include "utility/tinyformat.h"
#include <string>
#include <vector>

namespace souffle {

/**
 * @class AstBranchDeclaration
 * @brief Wrapper for the single branch declaration (product type) inside ADT declaration.
 *
 * A branch declaration corresponds to a product type and forms a part of ADT declaration.
 * Currently it's required for all the branches to have unique names.
 */
class AstBranchDeclaration : public AstNode {
public:
    AstBranchDeclaration(std::string name, VecOwn<AstAttribute> fields, SrcLocation loc = {})
            : AstNode(std::move(loc)), constructor(std::move(name)), fields(std::move(fields)){};

    const std::string& getConstructor() const {
        return constructor;
    }

    std::vector<AstAttribute*> getFields() {
        return toPtrVector(fields);
    }

    AstBranchDeclaration* clone() const override {
        return new AstBranchDeclaration(constructor, souffle::clone(fields), getSrcLoc());
    }

protected:
    void print(std::ostream& os) const override {
        os << tfm::format("%s {%s}", constructor, join(fields, ", "));
    }

private:
    std::string constructor;      // < the branch constructor
    VecOwn<AstAttribute> fields;  // < the branch fields
};

}  // namespace souffle
