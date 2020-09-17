/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SipsMetric.h
 *
 * Defines the SipsMetric class, which specifies cost functions for atom orderings in a clause.
 *
 ***********************************************************************/

#pragma once

#include <vector>

namespace souffle::ast {

class Atom;
class BindingStore;
class Clause;

class SipsMetric {
public:
    /**
     * Determines the new ordering of a clause after the SIPS is applied.
     * @param clause clause to reorder
     * @return the vector of new positions; v[i] = j iff atom j moves to pos i
     */
    std::vector<size_t> getReordering(const Clause* clause) const;

protected:
    virtual std::vector<float> evaluateCosts(
            const std::vector<Atom*> atoms, const BindingStore& bindingStore) const = 0;
};

class StrictSips : public SipsMetric {
protected:
    std::vector<float> evaluateCosts(
            const std::vector<Atom*> atoms, const BindingStore& bindingStore) const override;
};

class InputSips : public SipsMetric {
protected:
    std::vector<float> evaluateCosts(
            const std::vector<Atom*> atoms, const BindingStore& bindingStore) const override;
};

class ProfileUseSips : public SipsMetric {
protected:
    std::vector<float> evaluateCosts(
            const std::vector<Atom*> atoms, const BindingStore& bindingStore) const override;
};

};  // namespace souffle::ast
