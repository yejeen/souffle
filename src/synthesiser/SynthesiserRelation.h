/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "ram/Relation.h"
#include "ram/analysis/Index.h"
#include <cstddef>
#include <cstdint>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

namespace souffle {

class SynthesiserRelation {
public:
    SynthesiserRelation(const ram::Relation& rel, const ram::analysis::MinIndexSelection& indices,
            const bool isProvenance = false)
            : relation(rel), indices(indices), isProvenance(isProvenance) {}

    virtual ~SynthesiserRelation() = default;

    /** Compute the final list of indices to be used */
    virtual void computeIndices() = 0;

    /** Get arity of relation */
    size_t getArity() const {
        return relation.getArity();
    }

    /** Get data structure of relation */
    const std::string& getDataStructure() const {
        return dataStructure;
    }

    /** Get list of indices used for relation,
     * guaranteed that original indices in analysis::MinIndexSelection
     * come before any generated indices */
    ram::analysis::MinIndexSelection::OrderCollection getIndices() const {
        return computedIndices;
    }

    std::set<int> getProvenenceIndexNumbers() const {
        return provenanceIndexNumbers;
    }

    /** Get stored analysis::MinIndexSelection */
    const ram::analysis::MinIndexSelection& getMinIndexSelection() const {
        return indices;
    }

    /** Get stored ram::Relation */
    const ram::Relation& getRelation() const {
        return relation;
    }

    /** Print type name */
    virtual std::string getTypeName() = 0;

    /** Helper function to convert attribute types to a single string */
    std::string getTypeAttributeString(const std::vector<std::string>& attributeTypes,
            const std::unordered_set<uint32_t>& attributesUsed) const;

    /** Generate relation type struct */
    virtual void generateTypeStruct(std::ostream& out) = 0;

    /** Factory method to generate a SynthesiserRelation */
    static Own<SynthesiserRelation> getSynthesiserRelation(
            const ram::Relation& ramRel, const ram::analysis::MinIndexSelection& indexSet, bool isProvenance);

protected:
    /** Ram relation referred to by this */
    const ram::Relation& relation;

    /** Indices used for this relation */
    const ram::analysis::MinIndexSelection& indices;

    /** The data structure used for the relation */
    std::string dataStructure;

    /** The final list of indices used */
    ram::analysis::MinIndexSelection::OrderCollection computedIndices;

    /** The list of indices added for provenance computation */
    std::set<int> provenanceIndexNumbers;

    /** The number of the master index */
    size_t masterIndex = -1;

    /** Is this relation used with provenance */
    const bool isProvenance;
};

class SynthesiserNullaryRelation : public SynthesiserRelation {
public:
    SynthesiserNullaryRelation(
            const ram::Relation& ramRel, const ram::analysis::MinIndexSelection& indexSet, bool isProvenance)
            : SynthesiserRelation(ramRel, indexSet, isProvenance) {}

    void computeIndices() override;
    std::string getTypeName() override;
    void generateTypeStruct(std::ostream& out) override;
};

class SynthesiserInfoRelation : public SynthesiserRelation {
public:
    SynthesiserInfoRelation(
            const ram::Relation& ramRel, const ram::analysis::MinIndexSelection& indexSet, bool isProvenance)
            : SynthesiserRelation(ramRel, indexSet, isProvenance) {}

    void computeIndices() override;
    std::string getTypeName() override;
    void generateTypeStruct(std::ostream& out) override;
};

class SynthesiserDirectRelation : public SynthesiserRelation {
public:
    SynthesiserDirectRelation(
            const ram::Relation& ramRel, const ram::analysis::MinIndexSelection& indexSet, bool isProvenance)
            : SynthesiserRelation(ramRel, indexSet, isProvenance) {}

    void computeIndices() override;
    std::string getTypeName() override;
    void generateTypeStruct(std::ostream& out) override;
};

class SynthesiserIndirectRelation : public SynthesiserRelation {
public:
    SynthesiserIndirectRelation(
            const ram::Relation& ramRel, const ram::analysis::MinIndexSelection& indexSet, bool isProvenance)
            : SynthesiserRelation(ramRel, indexSet, isProvenance) {}

    void computeIndices() override;
    std::string getTypeName() override;
    void generateTypeStruct(std::ostream& out) override;
};

class SynthesiserBrieRelation : public SynthesiserRelation {
public:
    SynthesiserBrieRelation(
            const ram::Relation& ramRel, const ram::analysis::MinIndexSelection& indexSet, bool isProvenance)
            : SynthesiserRelation(ramRel, indexSet, isProvenance) {}

    void computeIndices() override;
    std::string getTypeName() override;
    void generateTypeStruct(std::ostream& out) override;
};

class SynthesiserEqrelRelation : public SynthesiserRelation {
public:
    SynthesiserEqrelRelation(
            const ram::Relation& ramRel, const ram::analysis::MinIndexSelection& indexSet, bool isProvenance)
            : SynthesiserRelation(ramRel, indexSet, isProvenance) {}

    void computeIndices() override;
    std::string getTypeName() override;
    void generateTypeStruct(std::ostream& out) override;
};
}  // end of namespace souffle
