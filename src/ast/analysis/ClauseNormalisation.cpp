#include "ast/analysis/ClauseNormalisation.h"
#include "ast/Atom.h"
#include "ast/Clause.h"
#include "ast/Literal.h"
#include "ast/QualifiedName.h"

namespace souffle {

class ClauseNormalisationAnalysis::NormalisedClauseRepr {
public:
    struct NormalisedClauseElementRepr {
        AstQualifiedName name;
        std::vector<std::string> params;
    };

    NormalisedClauseRepr() = default;

    NormalisedClauseRepr(const AstClause* clause) {
        // head
        AstQualifiedName name("@min:head");
        std::vector<std::string> headVars;
        for (const auto* arg : clause->getHead()->getArguments()) {
            headVars.push_back(normaliseArgument(arg));
        }
        clauseElements.push_back({.name = name, .params = headVars});

        // body
        for (const auto* lit : clause->getBodyLiterals()) {
            addClauseBodyLiteral("@min:scope:0", lit);
        }
    }

    bool isFullyNormalised() const {
        return fullyNormalised;
    }

    const std::set<std::string>& getVariables() const {
        return variables;
    }

    const std::set<std::string>& getConstants() const {
        return constants;
    }

    const std::vector<NormalisedClauseElementRepr>& getElements() const {
        return clauseElements;
    }

private:
    bool fullyNormalised{true};
    size_t aggrScopeCount{0};
    std::set<std::string> variables{};
    std::set<std::string> constants{};
    std::vector<NormalisedClauseElementRepr> clauseElements{};

    /**
     * Parse an atom with a preset name qualifier into the element list.
     */
    void addClauseAtom(const std::string& qualifier, const std::string& scopeID, const AstAtom* atom);

    /**
     * Parse a body literal into the element list.
     */
    void addClauseBodyLiteral(const std::string& scopeID, const AstLiteral* lit);

    /**
     * Return a normalised string repr of an argument.
     */
    std::string normaliseArgument(const AstArgument* arg);
};

void ClauseNormalisationAnalysis::NormalisedClauseRepr::addClauseAtom(
        const std::string& qualifier, const std::string& scopeID, const AstAtom* atom) {
    AstQualifiedName name(atom->getQualifiedName());
    name.prepend(qualifier);

    std::vector<std::string> vars;
    vars.push_back(scopeID);
    for (const auto* arg : atom->getArguments()) {
        vars.push_back(normaliseArgument(arg));
    }
    clauseElements.push_back({.name = name, .params = vars});
}

void ClauseNormalisationAnalysis::NormalisedClauseRepr::addClauseBodyLiteral(
        const std::string& scopeID, const AstLiteral* lit) {
    if (const auto* atom = dynamic_cast<const AstAtom*>(lit)) {
        addClauseAtom("@min:atom", scopeID, atom);
    } else if (const auto* neg = dynamic_cast<const AstNegation*>(lit)) {
        addClauseAtom("@min:neg", scopeID, neg->getAtom());
    } else if (const auto* bc = dynamic_cast<const AstBinaryConstraint*>(lit)) {
        AstQualifiedName name(toBinaryConstraintSymbol(bc->getOperator()));
        name.prepend("@min:operator");
        std::vector<std::string> vars;
        vars.push_back(scopeID);
        vars.push_back(normaliseArgument(bc->getLHS()));
        vars.push_back(normaliseArgument(bc->getRHS()));
        clauseElements.push_back({.name = name, .params = vars});
    } else {
        assert(lit != nullptr && "unexpected nullptr lit");
        fullyNormalised = false;
        std::stringstream qualifier;
        qualifier << "@min:unhandled:lit:" << scopeID;
        AstQualifiedName name(toString(*lit));
        name.prepend(qualifier.str());
        clauseElements.push_back({.name = name, .params = std::vector<std::string>()});
    }
}

std::string ClauseNormalisationAnalysis::NormalisedClauseRepr::normaliseArgument(const AstArgument* arg) {
    if (auto* stringCst = dynamic_cast<const AstStringConstant*>(arg)) {
        std::stringstream name;
        name << "@min:cst:str" << *stringCst;
        constants.insert(name.str());
        return name.str();
    } else if (auto* numericCst = dynamic_cast<const AstNumericConstant*>(arg)) {
        std::stringstream name;
        name << "@min:cst:num:" << *numericCst;
        constants.insert(name.str());
        return name.str();
    } else if (dynamic_cast<const AstNilConstant*>(arg) != nullptr) {
        constants.insert("@min:cst:nil");
        return "@min:cst:nil";
    } else if (auto* var = dynamic_cast<const AstVariable*>(arg)) {
        auto name = var->getName();
        variables.insert(name);
        return name;
    } else if (dynamic_cast<const AstUnnamedVariable*>(arg)) {
        static size_t countUnnamed = 0;
        std::stringstream name;
        name << "@min:unnamed:" << countUnnamed++;
        variables.insert(name.str());
        return name.str();
    } else if (auto* aggr = dynamic_cast<const AstAggregator*>(arg)) {
        // Set the scope to uniquely identify the aggregator
        std::stringstream scopeID;
        scopeID << "@min:scope:" << ++aggrScopeCount;
        variables.insert(scopeID.str());

        // Set the type signature of this aggregator
        std::stringstream aggrTypeSignature;
        aggrTypeSignature << "@min:aggrtype";
        std::vector<std::string> aggrTypeSignatureComponents;

        // - the operator is fixed and cannot be changed
        aggrTypeSignature << ":" << aggr->getOperator();

        // - the scope can be remapped as a variable
        aggrTypeSignatureComponents.push_back(scopeID.str());

        // - the normalised target expression can be remapped as a variable
        if (aggr->getTargetExpression() != nullptr) {
            std::string normalisedExpr = normaliseArgument(aggr->getTargetExpression());
            aggrTypeSignatureComponents.push_back(normalisedExpr);
        }

        // Type signature is its own special atom
        clauseElements.push_back({.name = aggrTypeSignature.str(), .params = aggrTypeSignatureComponents});

        // Add each contained normalised clause literal, tying it with the new scope ID
        for (const auto* literal : aggr->getBodyLiterals()) {
            addClauseBodyLiteral(scopeID.str(), literal);
        }

        // Aggregator identified by the scope ID
        return scopeID.str();
    } else {
        fullyNormalised = false;
        return "@min:unhandled:arg";
    }
}

void ClauseNormalisationAnalysis::run(const AstTranslationUnit& translationUnit) {
    // TODO
}

void print(std::ostream& os) const override {
    // TODO
}

}  // namespace souffle
