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
    AstBranchDeclaration(std::string n, VecOwn<AstAttribute> fs, SrcLocation l = {})
            : AstNode(std::move(l)), name(std::move(n)), fields(std::move(fs)){};

    bool operator==(const AstBranchDeclaration& other) const {
        return name == other.getName();
    }

    const std::string& getName() const {
        return name;
    }

    const AstQualifiedName& getType() const {
        return fields.at(0)->getTypeName();
    }

    AstBranchDeclaration* clone() const override {
        return new AstBranchDeclaration(name, souffle::clone(fields), getSrcLoc());
    }

protected:
    void print(std::ostream& os) const override {
        os << tfm::format("%s {%s}", name, join(fields, ", "));
    }

private:
    std::string name;             // < the branch name
    VecOwn<AstAttribute> fields;  // < the branch fields
};

}  // namespace souffle
