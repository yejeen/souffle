/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file NormaliseConstraints.h
 *
 * Transformation pass to normalise constraints.
 * E.g.: a(x) :- b(x, 1). -> a(x) :- b(x, tmp0), tmp0=1.
 *
 ***********************************************************************/

#pragma once

#include "ast/TranslationUnit.h"
#include "ast/transform/Transformer.h"
#include <string>

namespace souffle::ast::transform {

/**
 * Transformation pass to normalise constraints.
 * E.g.: a(x) :- b(x, 1). -> a(x) :- b(x, tmp0), tmp0=1.
 */
class NormaliseConstraintsTransformer : public Transformer {
public:
    std::string getName() const override {
        return "NormaliseConstraintsTransformer";
    }

    NormaliseConstraintsTransformer* clone() const override {
        return new NormaliseConstraintsTransformer();
    }

private:
    bool transform(TranslationUnit& translationUnit) override;
};

}  // namespace souffle::ast::transform
