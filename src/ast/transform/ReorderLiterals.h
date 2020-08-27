/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ReorderLiterals.h
 *
 ***********************************************************************/

#pragma once

#include "ast/transform/Transformer.h"
#include <functional>
#include <set>
#include <string>
#include <vector>

namespace souffle {

class AstAtom;
class AstClause;
class AstTranslationUnit;
class BindingStore;

/**
 * Type for SIPS functions
 * @param atoms a vector of (possibly nullptr) atoms to choose from
 * @param boundVariables a set of all already arguments already bound to a value
 * @return the index of the best atom to choose based on some SIPS-specific cost metric
 */
using old_sips_t = std::function<unsigned int(std::vector<AstAtom*>, const std::set<std::string>&)>;
using sips_t = std::function<unsigned int(std::vector<AstAtom*>, const BindingStore&)>;

/**
 * Transformation pass to reorder body literals.
 */
class ReorderLiteralsTransformer : public AstTransformer {
public:
    std::string getName() const override {
        return "ReorderLiteralsTransformer";
    }

    ReorderLiteralsTransformer* clone() const override {
        return new ReorderLiteralsTransformer();
    }

private:
    bool transform(AstTranslationUnit& translationUnit) override;

    /** Returns a SIPS function based on the SIPS option provided. */
    static old_sips_t getSipsFunction(const std::string& sipsChosen);

    /**
     * Reorder the clause based on a given SIPS function.
     * @param sipsFunction SIPS metric to use
     * @param clause clause to reorder
     * @return nullptr if no change, otherwise a new reordered clause
     */
    static AstClause* reorderClauseWithSips(old_sips_t sipsFunction, const AstClause* clause);

    /** Determines the new ordering of a vector of atoms after the given SIPS is applied. */
    static std::vector<unsigned int> applySips(old_sips_t sipsFunction, std::vector<AstAtom*> atoms);

    /** Count the number of bound arguments in a given atom */
    static unsigned int numBoundArguments(const AstAtom* atom, const std::set<std::string>& boundVariables);

    /** Count the number of bound arguments in a given atom */
    static unsigned int numBoundArguments(const AstAtom* atom, const BindingStore& bindingStore);
};

}  // end of namespace souffle
