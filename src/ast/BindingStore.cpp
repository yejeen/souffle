#include "ast/BindingStore.h"
#include "ast/BinaryConstraint.h"
#include "ast/Clause.h"
#include "ast/Constant.h"
#include "ast/Variable.h"
#include "ast/Visitor.h"

namespace souffle {

BindingStore::BindingStore(const AstClause* clause, const std::string& adornmentMarker) {
    /* Note that variables can be bound through:
     *  (1) an appearance in a body atom
     *  (2) an appearance in a bound field of the head atom
     *  (3) equality with a fully bound functor (via this class)
     *
     * When computing (3), appearances (1) and (2) must be separated to maintain the termination semantics of
     * the original program. Functor variables are not considered bound if they are only bound via the head.
     *
     * Justification: Suppose a new variable Y is marked as bound because of its appearance in a functor
     * Y=X+1, and X was already found to be bound:
     *  (1) If X was bound through a body atom, then the behaviour of typical magic-set is exhibited, where
     * the magic-set of Y is bounded by the values that X can take, which is bounded by induction.
     *  (2) If X was bound only through the head atom, then Y is only fixed to an appearance in a magic-atom.
     * In the presence of recursion, this can potentially lead to an infinitely-sized magic-set for an atom.
     */

    // Bind variables in the head that are marked as bound args
    const auto& headArgs = clause->getHead()->getArguments();
    for (size_t i = 0; i < adornmentMarker.length(); i++) {
        const auto* var = dynamic_cast<AstVariable*>(headArgs[i]);
        assert(var != nullptr && "expected only variables in head");
        if (adornmentMarker[i] == 'b') {
            boundHeadVariables.insert(var->getName());
        }
    }

    // Check through for variables bound in the body by a '<var> = <const>' term
    visitDepthFirst(*clause, [&](const AstBinaryConstraint& constr) {
        if (constr.getOperator() == BinaryConstraintOp::EQ && dynamic_cast<AstVariable*>(constr.getLHS()) &&
                dynamic_cast<AstConstant*>(constr.getRHS())) {
            const auto* var = dynamic_cast<AstVariable*>(constr.getLHS());
            bindVariable(var->getName());
        }
    });

    generateBindingDependencies(clause);
    reduceDependencies();
}

void BindingStore::processEqualityBindings(const AstArgument* lhs, const AstArgument* rhs) {
    // Only care about equalities affecting the bound status of variables
    const auto* var = dynamic_cast<const AstVariable*>(lhs);
    if (var == nullptr) return;

    // If all variables on the rhs are bound, then lhs is also bound
    BindingStore::ConjBindingSet depSet;
    visitDepthFirst(*rhs, [&](const AstVariable& subVar) { depSet.insert(subVar.getName()); });
    addBindingDependency(var->getName(), depSet);

    // If the lhs is bound, then all args in the rec on the rhs are also bound
    if (const auto* rec = dynamic_cast<const AstRecordInit*>(rhs)) {
        for (const auto* arg : rec->getArguments()) {
            const auto* subVar = dynamic_cast<const AstVariable*>(arg);
            assert(subVar != nullptr && "expected args to be variables");
            addBindingDependency(subVar->getName(), BindingStore::ConjBindingSet({var->getName()}));
        }
    }
}

void BindingStore::generateBindingDependencies(const AstClause* clause) {
    // Grab all relevant constraints (i.e. eq. constrs not involving aggregators)
    std::set<const AstBinaryConstraint*> constraints;
    visitDepthFirst(*clause, [&](const AstBinaryConstraint& bc) {
        bool containsAggregators = false;
        visitDepthFirst(bc, [&](const AstAggregator& /* aggr */) { containsAggregators = true; });
        if (!containsAggregators && bc.getOperator() == BinaryConstraintOp::EQ) {
            constraints.insert(&bc);
        }
    });

    // Add variable binding dependencies implied by the constraint
    for (const auto* bc : constraints) {
        processEqualityBindings(bc->getLHS(), bc->getRHS());
        processEqualityBindings(bc->getRHS(), bc->getLHS());
    }
}

BindingStore::ConjBindingSet BindingStore::reduceDependency(
        const BindingStore::ConjBindingSet& origDependency) {
    BindingStore::ConjBindingSet newDependency;
    for (const auto& var : origDependency) {
        // Only keep unbound variables in the dependency
        if (!contains(boundVariables, var)) {
            newDependency.insert(var);
        }
    }
    return newDependency;
}

BindingStore::DisjBindingSet BindingStore::reduceDependency(
        const BindingStore::DisjBindingSet& origDependency) {
    BindingStore::DisjBindingSet newDependencies;
    for (const auto& dep : origDependency) {
        auto newDep = reduceDependency(dep);
        if (!newDep.empty()) {
            newDependencies.insert(newDep);
        }
    }
    return newDependencies;
}

bool BindingStore::reduceDependencies() {
    bool changed = false;
    std::map<std::string, BindingStore::DisjBindingSet> newVariableDependencies;
    std::set<std::string> variablesToBind;

    // Reduce each variable's set of dependencies one by one
    for (const auto& [headVar, dependencies] : variableDependencies) {
        // No need to track the dependencies of already-bound variables
        if (contains(boundVariables, headVar)) {
            changed = true;
            continue;
        }

        // Reduce the dependency set based on bound variables
        auto newDependencies = reduceDependency(dependencies);
        if (newDependencies.empty() || newDependencies.size() < dependencies.size()) {
            // At least one dependency has been satisfied, so variable is now bound
            changed = true;
            variablesToBind.insert(headVar);
            continue;
        }
        newVariableDependencies[headVar] = newDependencies;
        changed |= (newDependencies != dependencies);
    }

    // Bind variables that need to be bound
    for (auto var : variablesToBind) {
        boundVariables.insert(var);
    }

    // Repeat it recursively if any changes happened, until we reach a fixpoint
    if (changed) {
        variableDependencies = newVariableDependencies;
        reduceDependencies();
        return true;
    }
    assert(variableDependencies == newVariableDependencies && "unexpected change");
    return false;
}

}  // namespace souffle
