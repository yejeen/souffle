/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All Rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "SrcLocation.h"
#include "ast/Argument.h"
#include "ast/Node.h"
#include "ast/NodeMapper.h"
#include "utility/ContainerUtil.h"
#include "utility/MiscUtil.h"
#include "utility/tinyformat.h"
#include <cassert>
#include <memory>

namespace souffle {

/**
 * ADT branch initialization
 */
class AstADTinit : public AstArgument {
public:
    AstADTinit(std::string branch, std::unique_ptr<AstArgument> arg, SrcLocation loc = {})
            : AstArgument(std::move(loc)), branch(std::move(branch)), arg(std::move(arg)) {
        assert(this->arg);
    }

    const AstArgument* getArgument() const {
        return arg.get();
    }

    const std::string& getBranch() const {
        return branch;
    }

    AstADTinit* clone() const override {
        return new AstADTinit(branch, souffle::clone(arg), getSrcLoc());
    }

    /** Mutates this node */
    void apply(const AstNodeMapper& map) override {
        arg = map(std::move(arg));
    }

    /** Obtains a list of all embedded child nodes */
    std::vector<const AstNode*> getChildNodes() const override {
        return {arg.get()};
    }

protected:
    void print(std::ostream& os) const override {
        os << tfm::format("$%s(%s)", branch, *arg);
    }

    /** Implements the node comparison for this node type */
    bool equal(const AstNode& node) const override {
        const auto& other = dynamic_cast<const AstADTinit&>(node);
        return (branch == other.branch) && equal_ptr(arg, other.arg);
    }

private:
    /** The sum type branch name */
    std::string branch;

    /** The argument */
    std::unique_ptr<AstArgument> arg;
};

}  // namespace souffle
