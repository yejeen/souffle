#pragma once

namespace souffle {

class ClauseNormalisationAnalysis : public AstAnalysis {
public:
    class NormalisedClauseRepr;

    static constexpr const char* name = "clause-normalisation";

    ClauseNormalisationAnalysis() : AstAnalysis(name) {}

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& os) const override;

    const NormalisedClauseRepr& getNormalisation(const AstClause* clause) const {
        assert(contains(normalisations, clause) && "clause not normalised");
        return normalisations.at(clause);
    }

private:
    std::map<const AstClause*, const NormalisedClauseRepr&> normalisations;
};

}  // namespace souffle
