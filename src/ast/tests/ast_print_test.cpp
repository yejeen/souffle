/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ast_print_test.cpp
 *
 * Tests souffle's AST program.
 *
 ***********************************************************************/

#include "tests/test.h"

#include "AggregateOp.h"
#include "ast/Aggregator.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/Clause.h"
#include "ast/Counter.h"
#include "ast/Literal.h"
#include "ast/NilConstant.h"
#include "ast/Node.h"
#include "ast/NumericConstant.h"
#include "ast/Program.h"
#include "ast/StringConstant.h"
#include "ast/TranslationUnit.h"
#include "ast/UnnamedVariable.h"
#include "ast/Variable.h"
#include "parser/ParserDriver.h"
#include "reports/DebugReport.h"
#include "reports/ErrorReport.h"
#include <algorithm>
#include <iosfwd>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace souffle::test {

inline Own<AstTranslationUnit> makeATU(std::string program = ".decl A,B,C(x:number)") {
    ErrorReport e;
    DebugReport d;
    return ParserDriver::parseTranslationUnit(program, e, d);
}

inline Own<AstTranslationUnit> makePrintedATU(Own<AstTranslationUnit>& tu) {
    std::stringstream ss;
    ss << *tu->getProgram();
    return makeATU(ss.str());
}

inline Own<AstClause> makeClauseA(Own<AstArgument> headArgument) {
    auto headAtom = mk<AstAtom>("A");
    headAtom->addArgument(std::move(headArgument));
    auto clause = mk<AstClause>();
    clause->setHead(std::move(headAtom));
    return clause;
}

TEST(AstPrint, NilConstant) {
    auto testArgument = mk<AstNilConstant>();

    auto tu1 = makeATU();
    tu1->getProgram()->addClause(makeClauseA(std::move(testArgument)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, NumberConstant) {
    auto testArgument = mk<AstNumericConstant>("2");

    EXPECT_EQ(testArgument, testArgument);

    auto tu1 = makeATU();
    tu1->getProgram()->addClause(makeClauseA(std::move(testArgument)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, StringConstant) {
    ErrorReport e;
    DebugReport d;
    auto testArgument = mk<AstStringConstant>("test string");

    auto tu1 = ParserDriver::parseTranslationUnit(".decl A,B,C(x:number)", e, d);
    tu1->getProgram()->addClause(makeClauseA(std::move(testArgument)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, Variable) {
    auto testArgument = mk<AstVariable>("testVar");

    auto tu1 = makeATU();
    tu1->getProgram()->addClause(makeClauseA(std::move(testArgument)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, UnnamedVariable) {
    auto testArgument = mk<AstUnnamedVariable>();

    auto tu1 = makeATU();
    tu1->getProgram()->addClause(makeClauseA(std::move(testArgument)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, Counter) {
    auto testArgument = mk<AstCounter>();

    auto tu1 = makeATU();
    tu1->getProgram()->addClause(makeClauseA(std::move(testArgument)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, AggregatorMin) {
    auto atom = mk<AstAtom>("B");
    atom->addArgument(mk<AstVariable>("x"));
    auto min = mk<AstAggregator>(AggregateOp::MIN, mk<AstVariable>("x"));

    VecOwn<AstLiteral> body;
    body.push_back(mk<AstAtom>("B"));

    min->setBody(std::move(body));

    auto tu1 = makeATU();
    auto* prog1 = tu1->getProgram();
    prog1->addClause(makeClauseA(std::move(min)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, AggregatorMax) {
    auto atom = mk<AstAtom>("B");
    atom->addArgument(mk<AstVariable>("x"));
    auto max = mk<AstAggregator>(AggregateOp::MAX, mk<AstVariable>("x"));

    VecOwn<AstLiteral> body;
    body.push_back(std::move(atom));
    max->setBody(std::move(body));

    auto tu1 = makeATU();
    auto* prog1 = tu1->getProgram();
    prog1->addClause(makeClauseA(std::move(max)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, AggregatorCount) {
    auto atom = mk<AstAtom>("B");
    atom->addArgument(mk<AstVariable>("x"));
    auto count = mk<AstAggregator>(AggregateOp::COUNT);

    VecOwn<AstLiteral> body;
    body.push_back(std::move(atom));
    count->setBody(std::move(body));

    auto tu1 = makeATU();
    auto* prog1 = tu1->getProgram();
    prog1->addClause(makeClauseA(std::move(count)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

TEST(AstPrint, AggregatorSum) {
    auto atom = mk<AstAtom>("B");
    atom->addArgument(mk<AstVariable>("x"));
    auto sum = mk<AstAggregator>(AggregateOp::SUM, mk<AstVariable>("x"));

    VecOwn<AstLiteral> body;
    body.push_back(std::move(atom));
    sum->setBody(std::move(body));

    auto tu1 = makeATU();
    auto* prog1 = tu1->getProgram();
    prog1->addClause(makeClauseA(std::move(sum)));
    auto tu2 = makePrintedATU(tu1);
    EXPECT_EQ(*tu1->getProgram(), *tu2->getProgram());
}

}  // end namespace souffle::test
