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

#include "ast/Node.h"
#include "ast/QualifiedName.h"
#include "parser/SrcLocation.h"
#include "utility/tinyformat.h"

namespace souffle {

class AstBranchDeclaration : public AstNode {
public:
    AstBranchDeclaration(std::string n, AstQualifiedName t, SrcLocation l = {})
            : AstNode(std::move(l)), name(std::move(n)), type(std::move(t)){};

    bool operator==(const AstBranchDeclaration& other) const {
        return name == other.getName() && type == other.getType();
    }

    const std::string& getName() const {
        return name;
    }

    const AstQualifiedName& getType() const {
        return type;
    }

    AstBranchDeclaration* clone() const override {
        return new AstBranchDeclaration(name, type, getSrcLoc());
    }

protected:
    void print(std::ostream& os) const override {
        os << tfm::format("%s {%s}", name, type);
    }

private:
    std::string name;       // < the branch name
    AstQualifiedName type;  // < the branch type
};

}  // namespace souffle
