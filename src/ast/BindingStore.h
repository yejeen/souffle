#pragma once

#include "souffle/utility/ContainerUtil.h"
#include <map>
#include <set>
#include <string>

namespace souffle {

class AstArgument;
class AstClause;

// Helper class for determining variable binding involving functors
/**
 * A storage of bound variables that dynamically determines the set of bound variables
 * within a clause.
 */
class BindingStore {
public:
    BindingStore(const AstClause* clause, const std::string& adornmentMarker);

    void bindVariable(std::string varName) {
        boundVariables.insert(varName);
        reduceDependencies();
    }

    bool isBound(std::string varName) const {
        return contains(boundVariables, varName) || contains(boundHeadVariables, varName);
    }

private:
    // Helper types to represent a disjunction of several dependency sets
    using ConjBindingSet = std::set<std::string>;
    using DisjBindingSet = std::set<ConjBindingSet>;

    std::set<std::string> boundVariables{};
    std::set<std::string> boundHeadVariables{};
    std::map<std::string, DisjBindingSet> variableDependencies{};

    /**
     * Add a new conjunction of variables as a potential binder for a given variable.
     * The variable is considered bound if all variables in the conjunction are bound.
     */
    void addBindingDependency(std::string variable, ConjBindingSet dependency) {
        if (!contains(variableDependencies, variable)) {
            variableDependencies[variable] = DisjBindingSet();
        }
        variableDependencies[variable].insert(dependency);
    }

    /** Add binding dependencies formed on lhs by a <lhs> = <rhs> equality constraint. */
    void processEqualityBindings(const AstArgument* lhs, const AstArgument* rhs);

    /** Generate all binding dependencies implied by the constraints within a given clause. */
    void generateBindingDependencies(const AstClause* clause);

    /** Reduce a conjunctive set of dependencies based on the current bound variable set. */
    ConjBindingSet reduceDependency(const ConjBindingSet& origDependency);

    /** Reduce a disjunctive set of variable dependencies based on the current bound variable set. */
    DisjBindingSet reduceDependency(const DisjBindingSet& origDependency);

    /** Reduce the full set of dependencies for all tracked variables, binding whatever needs to be bound. */
    bool reduceDependencies();
};

}  // namespace souffle
