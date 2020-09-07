/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ram_relation_test.cpp
 *
 * Tests arithmetic evaluation by the Interpreter.
 *
 ***********************************************************************/

#include "tests/test.h"

#include "Global.h"
#include "RelationTag.h"
#include "interpreter/InterpreterEngine.h"
#include "ram/Expression.h"
#include "ram/IO.h"
#include "ram/Program.h"
#include "ram/Project.h"
#include "ram/Query.h"
#include "ram/Relation.h"
#include "ram/Sequence.h"
#include "ram/SignedConstant.h"
#include "ram/Statement.h"
#include "ram/TranslationUnit.h"
#include "reports/DebugReport.h"
#include "reports/ErrorReport.h"
#include "souffle/RamTypes.h"
#include "souffle/SymbolTable.h"
#include "souffle/utility/json11.h"
#include <algorithm>
#include <cstddef>
#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace souffle::test {

using json11::Json;

#define RANDOM_TESTS 12

const std::string testInterpreterStore(std::vector<std::string> attribs,
        std::vector<std::string> attribsTypes, VecOwn<RamExpression> exprs) {
    Global::config().set("jobs", "1");

    const size_t arity = attribs.size();

    VecOwn<RamRelation> rels;
    Own<RamRelation> myrel =
            mk<RamRelation>("test", arity, 0, attribs, attribsTypes, RelationRepresentation::BTREE);

    Own<RamRelationReference> ref1 = mk<RamRelationReference>(myrel.get());
    Own<RamRelationReference> ref2 = mk<RamRelationReference>(myrel.get());

    Json types = Json::object{{"relation",
            Json::object{{"arity", static_cast<long long>(arity)}, {"auxArity", static_cast<long long>(0)},
                    {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> dirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};

    std::map<std::string, std::string> ioDirs = std::map<std::string, std::string>(dirs);

    Own<RamStatement> main = mk<RamSequence>(mk<RamQuery>(mk<RamProject>(std::move(ref1), std::move(exprs))),
            mk<RamIO>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<RamStatement>> subs;
    Own<RamProgram> prog = mk<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    Own<InterpreterEngine> interpreter = mk<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    return sout.str();
}

TEST(IO_store, FloatSimple) {
    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> attribsTypes = {"f", "f"};

    VecOwn<RamExpression> exprs;
    exprs.push_back(mk<RamSignedConstant>(ramBitCast(static_cast<RamFloat>(0.5))));
    exprs.push_back(mk<RamSignedConstant>(ramBitCast(static_cast<RamFloat>(0.5))));

    std::string expected = R"(---------------
test
===============
0.5	0.5
===============
)";

    auto result = testInterpreterStore(attribs, attribsTypes, std::move(exprs));
    EXPECT_EQ(expected, result);
}

TEST(IO_store, Signed) {
    std::vector<RamDomain> randomNumbers = testutil::generateRandomVector<RamDomain>(RANDOM_TESTS);

    // a0 a1 a2...
    std::vector<std::string> attribs(RANDOM_TESTS, "a");
    for (size_t i = 0; i < RANDOM_TESTS; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> attribsTypes(RANDOM_TESTS, "i");

    VecOwn<RamExpression> exprs;
    for (RamDomain i : randomNumbers) {
        exprs.push_back(mk<RamSignedConstant>(i));
    }

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    auto result = testInterpreterStore(attribs, attribsTypes, std::move(exprs));
    EXPECT_EQ(expected.str(), result);
}

TEST(IO_store, Float) {
    std::vector<RamFloat> randomNumbers = testutil::generateRandomVector<RamFloat>(RANDOM_TESTS);

    // a0 a1 a2...
    std::vector<std::string> attribs(RANDOM_TESTS, "a");
    for (size_t i = 0; i < RANDOM_TESTS; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> attribsTypes(RANDOM_TESTS, "f");

    VecOwn<RamExpression> exprs;
    for (RamFloat f : randomNumbers) {
        exprs.push_back(mk<RamSignedConstant>(ramBitCast(f)));
    }

    std::stringstream expected;
    expected << std::setprecision(std::numeric_limits<RamFloat>::max_digits10);

    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    auto result = testInterpreterStore(attribs, attribsTypes, std::move(exprs));
    EXPECT_EQ(expected.str(), result);
}

TEST(IO_store, Unsigned) {
    std::vector<RamUnsigned> randomNumbers = testutil::generateRandomVector<RamUnsigned>(RANDOM_TESTS);

    // a0 a1 a2...
    std::vector<std::string> attribs(RANDOM_TESTS, "a");
    for (size_t i = 0; i < RANDOM_TESTS; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> attribsTypes(RANDOM_TESTS, "u");

    VecOwn<RamExpression> exprs;
    for (RamUnsigned u : randomNumbers) {
        exprs.push_back(mk<RamSignedConstant>(ramBitCast(u)));
    }

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    auto result = testInterpreterStore(attribs, attribsTypes, std::move(exprs));
    EXPECT_EQ(expected.str(), result);
}

// Test (store) with different delimiter
TEST(IO_store, SignedChangedDelimiter) {
    std::vector<RamDomain> randomNumbers = testutil::generateRandomVector<RamDomain>(RANDOM_TESTS);
    const std::string delimiter{", "};

    Global::config().set("jobs", "1");

    VecOwn<RamRelation> rels;

    // a0 a1 a2...
    std::vector<std::string> attribs(RANDOM_TESTS, "a");
    for (size_t i = 0; i < RANDOM_TESTS; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> attribsTypes(RANDOM_TESTS, "i");

    Own<RamRelation> myrel =
            mk<RamRelation>("test", RANDOM_TESTS, 0, attribs, attribsTypes, RelationRepresentation::BTREE);
    Own<RamRelationReference> ref1 = mk<RamRelationReference>(myrel.get());
    Own<RamRelationReference> ref2 = mk<RamRelationReference>(myrel.get());

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"auxArity", static_cast<long long>(0)},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> dirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"delimiter", delimiter}, {"types", types.dump()}};

    std::map<std::string, std::string> ioDirs = std::map<std::string, std::string>(dirs);

    VecOwn<RamExpression> exprs;
    for (RamDomain i : randomNumbers) {
        exprs.push_back(mk<RamSignedConstant>(i));
    }

    Own<RamStatement> main = mk<RamSequence>(mk<RamQuery>(mk<RamProject>(std::move(ref1), std::move(exprs))),
            mk<RamIO>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<RamStatement>> subs;
    Own<RamProgram> prog = mk<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    Own<InterpreterEngine> interpreter = mk<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << delimiter << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_store, MixedTypes) {
    Global::config().set("jobs", "1");

    VecOwn<RamRelation> rels;

    std::vector<std::string> attribs{"t", "o", "s", "i", "a"};

    std::vector<std::string> attribsTypes{"i", "u", "f", "f", "s"};

    Own<RamRelation> myrel =
            mk<RamRelation>("test", 5, 0, attribs, attribsTypes, RelationRepresentation::BTREE);
    Own<RamRelationReference> ref1 = mk<RamRelationReference>(myrel.get());
    Own<RamRelationReference> ref2 = mk<RamRelationReference>(myrel.get());

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"auxArity", static_cast<long long>(0)},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> dirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> ioDirs = std::map<std::string, std::string>(dirs);

    SymbolTable symbolTable;
    ErrorReport errReport;
    DebugReport debugReport;

    VecOwn<RamExpression> exprs;
    RamFloat floatValue = 27.75;
    exprs.push_back(mk<RamSignedConstant>(3));
    exprs.push_back(mk<RamSignedConstant>(ramBitCast(static_cast<RamUnsigned>(27))));
    exprs.push_back(mk<RamSignedConstant>(ramBitCast(static_cast<RamFloat>(floatValue))));
    exprs.push_back(mk<RamSignedConstant>(ramBitCast(static_cast<RamFloat>(floatValue))));
    exprs.push_back(mk<RamSignedConstant>(symbolTable.lookup("meow")));

    Own<RamStatement> main = mk<RamSequence>(mk<RamQuery>(mk<RamProject>(std::move(ref1), std::move(exprs))),
            mk<RamIO>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<RamStatement>> subs;
    Own<RamProgram> prog = mk<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    RamTranslationUnit translationUnit(std::move(prog), symbolTable, errReport, debugReport);

    // configure and execute interpreter
    Own<InterpreterEngine> interpreter = mk<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << std::setprecision(std::numeric_limits<RamFloat>::max_digits10);
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << 3 << "\t" << 27 << "\t" << floatValue << "\t" << floatValue << "\t"
             << "meow"
             << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_load, Signed) {
    std::streambuf* backupCin = std::cin.rdbuf();
    std::istringstream testInput("5	3");
    std::cin.rdbuf(testInput.rdbuf());

    Global::config().set("jobs", "1");

    VecOwn<RamRelation> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> attribsTypes = {"i", "i"};
    Own<RamRelation> myrel =
            mk<RamRelation>("test", 2, 0, attribs, attribsTypes, RelationRepresentation::BTREE);
    Own<RamRelationReference> ref1 = mk<RamRelationReference>(myrel.get());
    Own<RamRelationReference> ref2 = mk<RamRelationReference>(myrel.get());

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"auxArity", static_cast<long long>(0)},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> readDirs = {{"operation", "input"}, {"IO", "stdin"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> readIoDirs = std::map<std::string, std::string>(readDirs);

    std::map<std::string, std::string> writeDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> writeIoDirs = std::map<std::string, std::string>(writeDirs);

    Own<RamStatement> main =
            mk<RamSequence>(mk<RamIO>(std::move(ref1), readIoDirs), mk<RamIO>(std::move(ref2), writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<RamStatement>> subs;
    Own<RamProgram> prog = mk<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    Own<InterpreterEngine> interpreter = mk<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
5	3
===============
)";
    EXPECT_EQ(expected, sout.str());

    std::cin.rdbuf(backupCin);
}

TEST(IO_load, Float) {
    std::streambuf* backupCin = std::cin.rdbuf();
    std::istringstream testInput("0.5	0.5");
    std::cin.rdbuf(testInput.rdbuf());

    Global::config().set("jobs", "1");

    VecOwn<RamRelation> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> attribsTypes = {"f", "f"};
    Own<RamRelation> myrel =
            mk<RamRelation>("test", 2, 0, attribs, attribsTypes, RelationRepresentation::BTREE);
    Own<RamRelationReference> ref1 = mk<RamRelationReference>(myrel.get());
    Own<RamRelationReference> ref2 = mk<RamRelationReference>(myrel.get());

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"auxArity", static_cast<long long>(0)},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> readDirs = {{"operation", "input"}, {"IO", "stdin"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> readIoDirs = std::map<std::string, std::string>(readDirs);

    std::map<std::string, std::string> writeDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> writeIoDirs = std::map<std::string, std::string>(writeDirs);

    Own<RamStatement> main =
            mk<RamSequence>(mk<RamIO>(std::move(ref1), readIoDirs), mk<RamIO>(std::move(ref2), writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<RamStatement>> subs;
    Own<RamProgram> prog = mk<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    Own<InterpreterEngine> interpreter = mk<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
0.5	0.5
===============
)";
    EXPECT_EQ(expected, sout.str());

    std::cin.rdbuf(backupCin);
}

TEST(IO_load, Unsigned) {
    std::streambuf* backupCin = std::cin.rdbuf();
    std::istringstream testInput("6	6");
    std::cin.rdbuf(testInput.rdbuf());

    Global::config().set("jobs", "1");

    VecOwn<RamRelation> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> attribsTypes = {"u", "u"};
    Own<RamRelation> myrel =
            mk<RamRelation>("test", 2, 0, attribs, attribsTypes, RelationRepresentation::BTREE);
    Own<RamRelationReference> ref1 = mk<RamRelationReference>(myrel.get());
    Own<RamRelationReference> ref2 = mk<RamRelationReference>(myrel.get());

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"auxArity", static_cast<long long>(0)},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> readDirs = {{"operation", "input"}, {"IO", "stdin"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> readIoDirs = std::map<std::string, std::string>(readDirs);

    std::map<std::string, std::string> writeDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> writeIoDirs = std::map<std::string, std::string>(writeDirs);

    Own<RamStatement> main =
            mk<RamSequence>(mk<RamIO>(std::move(ref1), readIoDirs), mk<RamIO>(std::move(ref2), writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<RamStatement>> subs;
    Own<RamProgram> prog = mk<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    Own<InterpreterEngine> interpreter = mk<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
6	6
===============
)";
    EXPECT_EQ(expected, sout.str());

    std::cin.rdbuf(backupCin);
}

TEST(IO_load, MixedTypesLoad) {
    std::streambuf* backupCin = std::cin.rdbuf();
    std::istringstream testInput("meow	-3	3	0.5");
    std::cin.rdbuf(testInput.rdbuf());

    Global::config().set("jobs", "1");

    VecOwn<RamRelation> rels;

    std::vector<std::string> attribs = {"l", "u", "b", "a"};
    std::vector<std::string> attribsTypes = {"s", "i", "u", "f"};
    Own<RamRelation> myrel =
            mk<RamRelation>("test", 4, 0, attribs, attribsTypes, RelationRepresentation::BTREE);
    Own<RamRelationReference> ref1 = mk<RamRelationReference>(myrel.get());
    Own<RamRelationReference> ref2 = mk<RamRelationReference>(myrel.get());

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"auxArity", static_cast<long long>(0)},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> readDirs = {{"operation", "input"}, {"IO", "stdin"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> readIoDirs = std::map<std::string, std::string>(readDirs);

    std::map<std::string, std::string> writeDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> writeIoDirs = std::map<std::string, std::string>(writeDirs);

    Own<RamStatement> main =
            mk<RamSequence>(mk<RamIO>(std::move(ref1), readIoDirs), mk<RamIO>(std::move(ref2), writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<RamStatement>> subs;
    Own<RamProgram> prog = mk<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    Own<InterpreterEngine> interpreter = mk<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
meow	-3	3	0.5
===============
)";

    EXPECT_EQ(expected, sout.str());

    std::cin.rdbuf(backupCin);
}

}  // end namespace souffle::test
