/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All Rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "ast/Type.h"

namespace souffle {

/**
 * Sum types aka tagged union
 */
class AstSumType : public AstType {
public:
    struct Branch {
        Branch() = default;
        Branch(std::string n, AstQualifiedName t, SrcLocation l = {})
                : name(std::move(n)), type(std::move(t)), loc(std::move(l)){};

        std::string name;       // < the branch name
        AstQualifiedName type;  // < the branch type
        SrcLocation loc;

        bool operator==(const Branch& other) const {
            return name == other.name && type == other.type;
        }

        friend std::ostream& operator<<(std::ostream& os, const Branch& br) {
            return os << tfm::format("%s {%s}", br.name, br.type);
        }
    };

    AstSumType(AstQualifiedName name, std::vector<Branch> bs, SrcLocation loc = {})
            : AstType(std::move(name), std::move(loc)), branches(std::move(bs)) {
        assert(!branches.empty());
    };

    const std::vector<Branch>& getBranches() const {
        return branches;
    }

    void print(std::ostream& os) const override {
        os << tfm::format(".type %s = %s", getQualifiedName(), join(branches, " ; "));
    }

    AstSumType* clone() const override {
        return new AstSumType(getQualifiedName(), branches, getSrcLoc());
    }

protected:
    bool equal(const AstNode& node) const override {
        const auto& other = dynamic_cast<const AstSumType&>(node);
        return getQualifiedName() == other.getQualifiedName() && branches == other.branches;
    }

private:
    /** The list of branches for this sum type. */
    std::vector<Branch> branches;
};

}  // namespace souffle
