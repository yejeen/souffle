/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ADTtoRecords.h
 *
 * Defines the desugaring of ADT to records.
 * Each record has a form [branch_id, [arguments]].
 *
 ***********************************************************************/

#pragma once

#include "ast/transform/Transformer.h"

namespace souffle {

class ADTtoRecords : public AstTransformer {
public:
    std::string getName() const override {
        return "ADTtoRecords";
    }

    ADTtoRecords* clone() const override {
        return new ADTtoRecords();
    }

private:
    bool transform(AstTranslationUnit& translationUnit) override;
};

}  // namespace souffle
