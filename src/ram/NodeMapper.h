/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file NodeMapper.h
 *
 * Declaration of RAM node and mappers for RAM nodes
 *
 ***********************************************************************/

#pragma once

#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include <cassert>
#include <memory>

namespace souffle {

class RamNode;

/**
 * @class RamNodeMapper
 * @brief An abstract class for manipulating RAM Nodes by substitution
 */
class RamNodeMapper {
public:
    virtual ~RamNodeMapper() = default;

    /**
     * @brief Abstract replacement method for a node.
     *
     * If the given nodes is to be replaced, the handed in node
     * will be destroyed by the mapper and the returned node
     * will become owned by the caller.
     */
    virtual Own<RamNode> operator()(Own<RamNode> node) const = 0;

    /**
     * @brief Wrapper for any subclass of the RAM node hierarchy performing type casts.
     */
    template <typename T>
    Own<T> operator()(Own<T> node) const {
        Own<RamNode> resPtr = (*this)(Own<RamNode>(static_cast<RamNode*>(node.release())));
        assert(isA<T>(resPtr.get()) && "Invalid target node!");
        return Own<T>(dynamic_cast<T*>(resPtr.release()));
    }
};

}  // end of namespace souffle
