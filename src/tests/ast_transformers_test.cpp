/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ast_transformers_test.cpp
 *
 * Tests souffle's AST transformers.
 *
 ***********************************************************************/

#include "tests/test.h"

#include "BinaryConstraintOps.h"
#include "DebugReport.h"
#include "ErrorReport.h"
#include "ParserDriver.h"
#include "ast/AstAbstract.h"
#include "ast/AstArgument.h"
#include "ast/AstClause.h"
#include "ast/AstLiteral.h"
#include "ast/AstNode.h"
#include "ast/AstProgram.h"
#include "ast/AstQualifiedName.h"
#include "ast/AstTranslationUnit.h"
#include "ast/AstUtils.h"
#include "ast/analysis/AstGround.h"
#include "ast/transform/MagicSet.h"
#include "ast/transform/MinimiseProgram.h"
#include "ast/transform/RemoveRedundantRelations.h"
#include "ast/transform/RemoveRelationCopies.h"
#include "ast/transform/ResolveAliases.h"
#include "utility/StringUtil.h"
#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

class AstRelation;

namespace test {

TEST(AstTransformers, GroundTermPropagation) {
    ErrorReport errorReport;
    DebugReport debugReport;
    // load some test program
    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
                .type D <: symbol
                .decl p(a:D,b:D)

                p(a,b) :- p(x,y), r = [x,y], s = r, s = [w,v], [w,v] = [a,b].
            )",
            errorReport, debugReport);

    AstProgram& program = *tu->getProgram();

    // check types in clauses
    AstClause* a = getClauses(program, "p")[0];

    EXPECT_EQ("p(a,b) :- \n   p(x,y),\n   r = [x,y],\n   s = r,\n   s = [w,v],\n   [w,v] = [a,b].",
            toString(*a));

    std::unique_ptr<AstClause> res = ResolveAliasesTransformer::resolveAliases(*a);
    std::unique_ptr<AstClause> cleaned = ResolveAliasesTransformer::removeTrivialEquality(*res);

    EXPECT_EQ(
            "p(x,y) :- \n   p(x,y),\n   [x,y] = [x,y],\n   [x,y] = [x,y],\n   [x,y] = [x,y],\n   [x,y] = "
            "[x,y].",
            toString(*res));
    EXPECT_EQ("p(x,y) :- \n   p(x,y).", toString(*cleaned));
}

TEST(AstTransformers, GroundTermPropagation2) {
    ErrorReport errorReport;
    DebugReport debugReport;
    // load some test program
    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
               .type D <: symbol
               .decl p(a:D,b:D)

               p(a,b) :- p(x,y), x = y, x = a, y = b.
           )",
            errorReport, debugReport);

    AstProgram& program = *tu->getProgram();

    // check types in clauses
    AstClause* a = getClauses(program, "p")[0];

    EXPECT_EQ("p(a,b) :- \n   p(x,y),\n   x = y,\n   x = a,\n   y = b.", toString(*a));

    std::unique_ptr<AstClause> res = ResolveAliasesTransformer::resolveAliases(*a);
    std::unique_ptr<AstClause> cleaned = ResolveAliasesTransformer::removeTrivialEquality(*res);

    EXPECT_EQ("p(b,b) :- \n   p(b,b),\n   b = b,\n   b = b,\n   b = b.", toString(*res));
    EXPECT_EQ("p(b,b) :- \n   p(b,b).", toString(*cleaned));
}

TEST(AstTransformers, ResolveGroundedAliases) {
    // load some test program
    ErrorReport errorReport;
    DebugReport debugReport;
    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
                .type D <: symbol
                .decl p(a:D,b:D)

                p(a,b) :- p(x,y), r = [x,y], s = r, s = [w,v], [w,v] = [a,b].
            )",
            errorReport, debugReport);

    AstProgram& program = *tu->getProgram();

    EXPECT_EQ("p(a,b) :- \n   p(x,y),\n   r = [x,y],\n   s = r,\n   s = [w,v],\n   [w,v] = [a,b].",
            toString(*getClauses(program, "p")[0]));

    std::make_unique<ResolveAliasesTransformer>()->apply(*tu);

    EXPECT_EQ("p(x,y) :- \n   p(x,y).", toString(*getClauses(program, "p")[0]));
}

TEST(AstTransformers, ResolveAliasesWithTermsInAtoms) {
    // load some test program
    ErrorReport errorReport;
    DebugReport debugReport;
    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
                .type D <: symbol
                .decl p(a:D,b:D)

                p(x,c) :- p(x,b), p(b,c), c = b+1, x=c+2.
            )",
            errorReport, debugReport);

    AstProgram& program = *tu->getProgram();

    EXPECT_EQ("p(x,c) :- \n   p(x,b),\n   p(b,c),\n   c = (b+1),\n   x = (c+2).",
            toString(*getClauses(program, "p")[0]));

    std::make_unique<ResolveAliasesTransformer>()->apply(*tu);

    EXPECT_EQ("p(x,c) :- \n   p(x,b),\n   p(b,c),\n   c = (b+1),\n   x = (c+2).",
            toString(*getClauses(program, "p")[0]));
}

/**
 * Test that copies of relations are removed by RemoveRelationCopiesTransformer
 *
 * A(1, 2).
 * B(x, y) :- A(x, y).
 * C(x, y) :- B(x, y).
 * D(x, y) :- C(x, y).
 *
 * -> D(x, y) :- A(x, y).
 *
 *  B and C can be removed
 *
 */

TEST(AstTransformers, RemoveRelationCopies) {
    ErrorReport errorReport;
    DebugReport debugReport;
    // load some test program
    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
                .type D = number
                .decl a(a:D,b:D)
                .decl b(a:D,b:D)
                .decl c(a:D,b:D)
                .decl d(a:D,b:D)

                a(1,2).
                b(x,y) :- a(x,y).
                c(x,y) :- b(x,y).

                d(x,y) :- b(x,y), c(y,x).

            )",
            errorReport, debugReport);

    AstProgram& program = *tu->getProgram();

    EXPECT_EQ(4, program.getRelations().size());

    RemoveRelationCopiesTransformer::removeRelationCopies(*tu);

    EXPECT_EQ(2, program.getRelations().size());
}

/**
 * Test that copies of relations are removed by RemoveRelationCopiesTransformer
 *
 * A(1, 2).
 * B(x, y) :- A(x, y).
 * C(x, y) :- B(x, y).
 * D(x, y) :- C(x, y).
 * .output C
 *
 * -> C(x, y) :- A(x, y).
 * -> D(x, y) :- C(x, y).
 *
 *  B can be removed, but not C as C is output
 *
 */
TEST(AstTransformers, RemoveRelationCopiesOutput) {
    ErrorReport errorReport;
    DebugReport debugReport;
    // load some test program
    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
                .type D = number
                .decl a(a:D,b:D)
                .decl b(a:D,b:D)
                .decl c(a:D,b:D)
                .output c
                .decl d(a:D,b:D)

                a(1,2).
                b(x,y) :- a(x,y).
                c(x,y) :- b(x,y).

                d(x,y) :- b(x,y), c(y,x).

            )",
            errorReport, debugReport);

    AstProgram& program = *tu->getProgram();

    EXPECT_EQ(4, program.getRelations().size());

    RemoveRelationCopiesTransformer::removeRelationCopies(*tu);

    EXPECT_EQ(3, program.getRelations().size());
}

/**
 * Test the equivalence (or lack of equivalence) of clauses using the MinimiseProgramTransfomer.
 */
TEST(AstTransformers, CheckClausalEquivalence) {
    ErrorReport errorReport;
    DebugReport debugReport;

    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
                .decl A(x:number, y:number)
                .decl B(x:number)
                .decl C(x:number)

                A(0,0).
                A(0,0).
                A(0,1).

                B(1).

                C(z) :- A(z,y), A(z,x), x != 3, x < y, !B(x), y > 3, B(y).
                C(r) :- A(r,y), A(r,x), x != 3, x < y, !B(y), y > 3, B(y), B(x), x < y.
                C(x) :- A(x,a), a != 3, !B(a), A(x,b), b > 3, B(c), a < b, c = b.
            )",
            errorReport, debugReport);

    const auto& program = *tu->getProgram();

    // Resolve aliases to remove trivial equalities
    std::make_unique<ResolveAliasesTransformer>()->apply(*tu);
    auto aClauses = getClauses(program, "A");
    auto bClauses = getClauses(program, "B");
    auto cClauses = getClauses(program, "C");

    EXPECT_EQ(3, aClauses.size());
    EXPECT_EQ("A(0,0).", toString(*aClauses[0]));
    EXPECT_EQ("A(0,0).", toString(*aClauses[1]));
    EXPECT_EQ("A(0,1).", toString(*aClauses[2]));

    EXPECT_EQ(1, bClauses.size());
    EXPECT_EQ("B(1).", toString(*bClauses[0]));

    EXPECT_EQ(3, cClauses.size());
    EXPECT_EQ("C(z) :- \n   A(z,y),\n   A(z,x),\n   x != 3,\n   x < y,\n   !B(x),\n   y > 3,\n   B(y).",
            toString(*cClauses[0]));
    EXPECT_EQ(
            "C(r) :- \n   A(r,y),\n   A(r,x),\n   x != 3,\n   x < y,\n   !B(y),\n   y > 3,\n   B(y),\n   "
            "B(x).",
            toString(*cClauses[1]));
    EXPECT_EQ("C(x) :- \n   A(x,a),\n   a != 3,\n   !B(a),\n   A(x,b),\n   b > 3,\n   B(b),\n   a < b.",
            toString(*cClauses[2]));

    // Check equivalence of these clauses
    // -- A
    EXPECT_TRUE(MinimiseProgramTransformer::areBijectivelyEquivalent(aClauses[0], aClauses[1]));
    EXPECT_TRUE(MinimiseProgramTransformer::areBijectivelyEquivalent(aClauses[1], aClauses[0]));
    EXPECT_FALSE(MinimiseProgramTransformer::areBijectivelyEquivalent(aClauses[1], aClauses[2]));
    EXPECT_FALSE(MinimiseProgramTransformer::areBijectivelyEquivalent(aClauses[0], aClauses[2]));

    // -- C
    EXPECT_TRUE(MinimiseProgramTransformer::areBijectivelyEquivalent(cClauses[0], cClauses[2]));
    EXPECT_FALSE(MinimiseProgramTransformer::areBijectivelyEquivalent(cClauses[0], cClauses[1]));
    EXPECT_FALSE(MinimiseProgramTransformer::areBijectivelyEquivalent(cClauses[2], cClauses[1]));

    // Make sure equivalent (and only equivalent) clauses are removed by the minimiser
    std::make_unique<MinimiseProgramTransformer>()->apply(*tu);
    auto aMinClauses = getClauses(program, "A");
    auto bMinClauses = getClauses(program, "B");
    auto cMinClauses = getClauses(program, "C");

    EXPECT_EQ(2, aMinClauses.size());
    EXPECT_EQ("A(0,0).", toString(*aMinClauses[0]));
    EXPECT_EQ("A(0,1).", toString(*aMinClauses[1]));

    EXPECT_EQ(1, bMinClauses.size());
    EXPECT_EQ("B(1).", toString(*bMinClauses[0]));

    EXPECT_EQ(2, cMinClauses.size());
    EXPECT_EQ("C(z) :- \n   A(z,y),\n   A(z,x),\n   x != 3,\n   x < y,\n   !B(x),\n   y > 3,\n   B(y).",
            toString(*cMinClauses[0]));
    EXPECT_EQ(
            "C(r) :- \n   A(r,y),\n   A(r,x),\n   x != 3,\n   x < y,\n   !B(y),\n   y > 3,\n   B(y),\n   "
            "B(x).",
            toString(*cMinClauses[1]));
}

/**
 * Test the removal of redundancies within clauses using the MinimiseProgramTransformer.
 *
 * In particular, the removal of:
 *      - intraclausal literals equivalent to another literal in the body
 *          e.g. a(x) :- b(x), b(x), c(x). --> a(x) :- b(x), c(x).
 *      - clauses that are only trivially satisfiable
 *          e.g. a(x) :- a(x), x != 0. is only true if a(x) is already true
 */
TEST(AstTransformers, RemoveClauseRedundancies) {
    ErrorReport errorReport;
    DebugReport debugReport;

    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
                .decl a,b,c(X:number)
                a(0).
                b(1).
                c(X) :- b(X).

                a(X) :- b(X), c(X).
                a(X) :- a(X).
                a(X) :- a(X), X != 1.

                q(X) :- a(X).

                .decl q(X:number)
                .output q()
            )",
            errorReport, debugReport);

    const auto& program = *tu->getProgram();

    // Invoking the `RemoveRelationCopiesTransformer` to create some extra redundancy
    // In particular: The relation `c` will be replaced with `b` throughout, creating
    // the clause b(x) :- b(x).
    std::make_unique<RemoveRelationCopiesTransformer>()->apply(*tu);
    EXPECT_EQ(nullptr, getRelation(program, "c"));
    auto bIntermediateClauses = getClauses(program, "b");
    EXPECT_EQ(2, bIntermediateClauses.size());
    EXPECT_EQ("b(1).", toString(*bIntermediateClauses[0]));
    EXPECT_EQ("b(X) :- \n   b(X).", toString(*bIntermediateClauses[1]));

    // Attempt to minimise the program
    std::make_unique<MinimiseProgramTransformer>()->apply(*tu);
    EXPECT_EQ(3, program.getRelations().size());

    auto aClauses = getClauses(program, "a");
    EXPECT_EQ(2, aClauses.size());
    EXPECT_EQ("a(0).", toString(*aClauses[0]));
    EXPECT_EQ("a(X) :- \n   b(X).", toString(*aClauses[1]));

    auto bClauses = getClauses(program, "b");
    EXPECT_EQ(1, bClauses.size());
    EXPECT_EQ("b(1).", toString(*bClauses[0]));

    auto qClauses = getClauses(program, "q");
    EXPECT_EQ(1, qClauses.size());
    EXPECT_EQ("q(X) :- \n   a(X).", toString(*qClauses[0]));
}

/**
 * Test the magic-set transformation on an example that covers all subtransformers, namely:
 *      (1) NormaliseDatabaseTransformer
 *      (2) LabelDatabaseTransformer
 *      (3) AdornDatabaseTransformer
 *      (4) MagicSetTransformer
 */
TEST(AstTransformers, MagicSetComprehensive) {
    ErrorReport e;
    DebugReport d;

    std::unique_ptr<AstTranslationUnit> tu = ParserDriver::parseTranslationUnit(
            R"(
                // Stratum 0 - Base Relations
                .decl BaseOne(X:number) magic
                .decl BaseTwo(X:number) magic
                .input BaseOne, BaseTwo

                // Stratum 1 [depends on: 0]
                .decl A(X:number) magic
                .decl B(X:number) magic
                A(X) :- BaseOne(X).
                A(X) :- BaseOne(X), B(X).
                B(X) :- BaseTwo(X), A(X).

                // Stratum 2 [depends on: 0,1]
                .decl C(X:number) magic
                C(X) :- BaseTwo(X), A(X), B(X), X != 1.

                // Stratum 3 [depends on: 0,1]
                .decl R(X:number) magic
                R(X) :- BaseTwo(X), A(X), B(X), X != 0.

                // Stratum 4 [depends on: 0,1,2,3]
                .decl D(X:number) magic
                D(X) :- BaseOne(X), A(X), !C(X), !R(X).

                // Stratum 4 - Query [depends on: 0,1,4]
                .decl Query(X:number) magic
                .output Query
                Query(X) :- BaseOne(X), D(X), A(X).
            )",
            e, d);

    const auto& program = *tu->getProgram();

    /* Stage 1: Database normalisation */
    // Constants should now be extracted from the inequality constraints
    std::make_unique<MagicSetTransformer::NormaliseDatabaseTransformer>()->apply(*tu);
    const auto relations1 = program.getRelations();
    EXPECT_EQ(8, relations1.size());
    EXPECT_EQ(7, program.getClauses().size());

    const auto aClauses1 = getClauses(program, "A");
    EXPECT_EQ(2, aClauses1.size());
    EXPECT_EQ("A(X) :- \n   BaseOne(X).", toString(*aClauses1[0]));
    EXPECT_EQ("A(X) :- \n   BaseOne(X),\n   B(X).", toString(*aClauses1[1]));

    const auto bClauses1 = getClauses(program, "B");
    EXPECT_EQ(1, bClauses1.size());
    EXPECT_EQ("B(X) :- \n   BaseTwo(X),\n   A(X).", toString(*bClauses1[0]));

    const auto qClauses1 = getClauses(program, "Query");
    EXPECT_EQ(1, qClauses1.size());
    EXPECT_EQ("Query(X) :- \n   BaseOne(X),\n   D(X),\n   A(X).", toString(*qClauses1[0]));

    const auto cClauses1 = getClauses(program, "C");
    EXPECT_EQ(1, cClauses1.size());
    EXPECT_EQ("C(X) :- \n   BaseTwo(X),\n   A(X),\n   B(X),\n   X != @abdul0,\n   @abdul0 = 1.",
            toString(*cClauses1[0]));

    const auto rClauses1 = getClauses(program, "R");
    EXPECT_EQ(1, rClauses1.size());
    EXPECT_EQ("R(X) :- \n   BaseTwo(X),\n   A(X),\n   B(X),\n   X != @abdul0,\n   @abdul0 = 0.",
            toString(*rClauses1[0]));

    const auto dClauses1 = getClauses(program, "D");
    EXPECT_EQ(1, dClauses1.size());
    EXPECT_EQ("D(X) :- \n   BaseOne(X),\n   A(X),\n   !C(X),\n   !R(X).", toString(*dClauses1[0]));

    /* Stage 2: Database negative and positive labelling, keeping only the relevant ones */
    /* Stage 2.1: Negative labelling */
    std::make_unique<MagicSetTransformer::LabelDatabaseTransformer::NegativeLabellingTransformer>()->apply(
            *tu);
    const auto relations2 = program.getRelations();
    EXPECT_EQ(14, relations2.size());
    EXPECT_EQ(14, program.getClauses().size());

    // Original strata - neglabels should appear on all negated appearances of relations
    const auto aClauses2 = getClauses(program, "A");
    EXPECT_EQ(2, aClauses2.size());
    EXPECT_EQ("A(X) :- \n   BaseOne(X).", toString(*aClauses2[0]));
    EXPECT_EQ("A(X) :- \n   BaseOne(X),\n   B(X).", toString(*aClauses2[1]));

    const auto bClauses2 = getClauses(program, "B");
    EXPECT_EQ(1, bClauses2.size());
    EXPECT_EQ("B(X) :- \n   BaseTwo(X),\n   A(X).", toString(*bClauses2[0]));

    const auto qClauses2 = getClauses(program, "Query");
    EXPECT_EQ(1, qClauses2.size());
    EXPECT_EQ("Query(X) :- \n   BaseOne(X),\n   D(X),\n   A(X).", toString(*qClauses2[0]));

    const auto cClauses2 = getClauses(program, "C");
    EXPECT_EQ(1, cClauses2.size());
    EXPECT_EQ("C(X) :- \n   BaseTwo(X),\n   A(X),\n   B(X),\n   X != @abdul0,\n   @abdul0 = 1.",
            toString(*cClauses2[0]));

    const auto rClauses2 = getClauses(program, "R");
    EXPECT_EQ(1, rClauses2.size());
    EXPECT_EQ("R(X) :- \n   BaseTwo(X),\n   A(X),\n   B(X),\n   X != @abdul0,\n   @abdul0 = 0.",
            toString(*rClauses2[0]));

    const auto dClauses2 = getClauses(program, "D");
    EXPECT_EQ(1, dClauses2.size());
    EXPECT_EQ("D(X) :- \n   BaseOne(X),\n   A(X),\n   !@neglabel.C(X),\n   !@neglabel.R(X).",
            toString(*dClauses2[0]));

    // Neglaelled strata - relations should be copied and neglabelled one stratum at a time
    const auto negaClauses2 =
            getClauses(program, AstQualifiedName(std::vector<std::string>({"@neglabel", "A"})));
    EXPECT_EQ(2, negaClauses2.size());
    EXPECT_EQ("@neglabel.A(X) :- \n   BaseOne(X).", toString(*negaClauses2[0]));
    EXPECT_EQ("@neglabel.A(X) :- \n   BaseOne(X),\n   @neglabel.B(X).", toString(*negaClauses2[1]));

    const auto negbClauses2 =
            getClauses(program, AstQualifiedName(std::vector<std::string>({"@neglabel", "B"})));
    EXPECT_EQ(1, negbClauses2.size());
    EXPECT_EQ("@neglabel.B(X) :- \n   BaseTwo(X),\n   @neglabel.A(X).", toString(*negbClauses2[0]));

    const auto negqClauses2 =
            getClauses(program, AstQualifiedName(std::vector<std::string>({"@neglabel", "Query"})));
    EXPECT_EQ(1, negqClauses2.size());
    EXPECT_EQ("@neglabel.Query(X) :- \n   BaseOne(X),\n   D(X),\n   A(X).", toString(*negqClauses2[0]));

    const auto negcClauses2 =
            getClauses(program, AstQualifiedName(std::vector<std::string>({"@neglabel", "C"})));
    EXPECT_EQ(1, negcClauses2.size());
    EXPECT_EQ("@neglabel.C(X) :- \n   BaseTwo(X),\n   A(X),\n   B(X),\n   X != @abdul0,\n   @abdul0 = 1.",
            toString(*negcClauses2[0]));

    const auto negrClauses2 =
            getClauses(program, AstQualifiedName(std::vector<std::string>({"@neglabel", "R"})));
    EXPECT_EQ(1, negrClauses2.size());
    EXPECT_EQ("@neglabel.R(X) :- \n   BaseTwo(X),\n   A(X),\n   B(X),\n   X != @abdul0,\n   @abdul0 = 0.",
            toString(*negrClauses2[0]));

    const auto negdClauses2 =
            getClauses(program, AstQualifiedName(std::vector<std::string>({"@neglabel", "D"})));
    EXPECT_EQ(1, negdClauses2.size());
    EXPECT_EQ("@neglabel.D(X) :- \n   BaseOne(X),\n   A(X),\n   !@neglabel.C(X),\n   !@neglabel.R(X).",
            toString(*negdClauses2[0]));

    /* Stage 2.2: Positive labelling */
    std::make_unique<MagicSetTransformer::LabelDatabaseTransformer::PositiveLabellingTransformer>()->apply(
            *tu);
    const auto relations3 = program.getRelations();
    EXPECT_EQ(33, relations3.size());
    EXPECT_EQ(27, program.getClauses().size());

    // TODO: poslabel results

    /* Stage 2.3: Remove unnecessary labelled relations */
    std::make_unique<RemoveRedundantRelationsTransformer>()->apply(*tu);
    const auto relations4 = program.getRelations();
    EXPECT_EQ(12, relations4.size());
    EXPECT_EQ(13, program.getClauses().size());

    // TODO: poslabel results after removal

    /* Stage 3: Database adornment */
    std::make_unique<MagicSetTransformer::AdornDatabaseTransformer>()->apply(*tu);
    const auto relations5 = program.getRelations();
    // TODO: fix poscopy magic being ignored
    // EXPECT_EQ(19, relations5.size());
    // EXPECT_EQ(23, program.getClauses().size());

    // TODO: adornment results

    /* Stage 4: MST core transformation */
    std::make_unique<MagicSetTransformer::MagicSetCoreTransformer>()->apply(*tu);
    // TODO: mst core results
}

}  // namespace test
}  // namespace souffle
