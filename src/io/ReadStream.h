/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ReadStream.h
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"
#include "RecordTable.h"
#include "SerialisationStream.h"
#include "SymbolTable.h"
#include "json11.h"
#include "utility/ContainerUtil.h"
#include "utility/MiscUtil.h"
#include "utility/StringUtil.h"
#include <cctype>
#include <cstddef>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace souffle {

class ReadStream : public SerialisationStream<false> {
protected:
    ReadStream(
            const std::map<std::string, std::string>& rwOperation, SymbolTable& symTab, RecordTable& recTab)
            : SerialisationStream(symTab, recTab, rwOperation) {}

public:
    template <typename T>
    void readAll(T& relation) {
        auto lease = symbolTable.acquireLock();
        (void)lease;
        while (const auto next = readNextTuple()) {
            const RamDomain* ramDomain = next.get();
            relation.insert(ramDomain);
        }
    }

protected:
    /**
     * Read a record from a string.
     *
     * @param source - string containing a record
     * @param recordTypeName - record type.
     * @parem pos - start parsing from this position.
     * @param consumed - if not nullptr: number of characters read.
     *
     */
    RamDomain readRecord(const std::string& source, const std::string& recordTypeName, size_t pos = 0,
            size_t* charactersRead = nullptr) {
        const size_t initial_position = pos;

        // Check if record type information are present
        auto&& recordInfo = types["records"][recordTypeName];
        if (recordInfo.is_null()) {
            throw std::invalid_argument("Missing record type information: " + recordTypeName);
        }

        // Handle nil case
        consumeWhiteSpace(source, pos);
        if (source.substr(pos, 3) == "nil") {
            if (charactersRead != nullptr) {
                *charactersRead = 3;
            }
            return 0;
        }

        auto&& recordTypes = recordInfo["types"];
        const size_t recordArity = recordInfo["arity"].long_value();

        std::vector<RamDomain> recordValues(recordArity);

        consumeChar(source, '[', pos);

        for (size_t i = 0; i < recordArity; ++i) {
            const std::string& recordType = recordTypes[i].string_value();
            size_t consumed = 0;

            if (i > 0) {
                consumeChar(source, ',', pos);
            }
            consumeWhiteSpace(source, pos);
            switch (recordType[0]) {
                case 's': {
                    recordValues[i] = symbolTable.unsafeLookup(readUntil(source, ",]", pos, &consumed));
                    break;
                }
                case 'i': {
                    recordValues[i] = RamSignedFromString(source.substr(pos), &consumed);
                    break;
                }
                case 'u': {
                    recordValues[i] = ramBitCast(RamUnsignedFromString(source.substr(pos), &consumed));
                    break;
                }
                case 'f': {
                    recordValues[i] = ramBitCast(RamFloatFromString(source.substr(pos), &consumed));
                    break;
                }
                case 'r': {
                    recordValues[i] = readRecord(source, recordType, pos, &consumed);
                    break;
                }
                case '+': {
                    recordValues[i] = readADT(source, recordType, pos, &consumed);
                    break;
                }
                default: fatal("Invalid type attribute");
            }
            pos += consumed;
        }
        consumeChar(source, ']', pos);

        if (charactersRead != nullptr) {
            *charactersRead = pos - initial_position;
        }

        return recordTable.pack(recordValues.data(), recordValues.size());
    }

    RamDomain readADT(const std::string& source, const std::string& adtName, size_t pos = 0,
            size_t* charactersRead = nullptr) {
        const size_t initial_position = pos;

        // Branch will be encoded as (branchIdx, branchValue).
        RamDomain branchIdx = -1;
        RamDomain branchValue;

        auto&& adtInfo = types["ADTs"][adtName];
        const auto& branches = adtInfo["branches"];

        if (adtInfo.is_null() || !branches.is_array()) {
            throw std::invalid_argument("Missing ADT information: " + adtName);
        }

        // Consume initial character
        consumeChar(source, '$', pos);
        size_t consumed = 0;
        std::string branchName = readUntil(source, "(", pos, &consumed);  // You need to move pos.
        pos += consumed;

        consumeChar(source, '(', pos);

        std::optional<json11::Json> branchInfo = [&]() -> std::optional<json11::Json> {
            for (auto branch : branches.array_items()) {
                ++branchIdx;
                if (branch["name"].string_value() == branchName) {
                    return branch;
                }
            }
            return {};
        }();

        if (!branchInfo.has_value()) {
            throw std::invalid_argument("Missing branch information: " + branchName);
        }

        auto branchType = branchInfo.value()["type"].string_value();
        assert(!branchType.empty());

        consumed = 0;

        switch (branchType[0]) {
            case 's': {
                branchValue = symbolTable.unsafeLookup(readUntil(source, ")", pos, &consumed));
                break;
            }
            case 'i': {
                branchValue = RamSignedFromString(source.substr(pos), &consumed);
                break;
            }
            case 'u': {
                branchValue = ramBitCast(RamUnsignedFromString(source.substr(pos), &consumed));
                break;
            }
            case 'f': {
                branchValue = ramBitCast(RamFloatFromString(source.substr(pos), &consumed));
                break;
            }
            case 'r': {
                branchValue = readRecord(source, branchType, pos, &consumed);
                break;
            }
            case '+': {
                branchValue = readADT(source, branchType, pos, &consumed);
                break;
            }
            default: fatal("Invalid type attribute");
        }
        pos += consumed;

        consumeChar(source, ')', pos);

        if (charactersRead != nullptr) {
            *charactersRead = pos - initial_position;
        }
        return recordTable.pack(toVector<RamDomain>(branchIdx, branchValue).data(), 2);
    }

    std::string readUntil(const std::string& source, const std::string stopChars, const size_t pos,
            size_t* charactersRead) {
        size_t endOfSymbol = source.find_first_of(stopChars, pos);

        if (endOfSymbol == std::string::npos) {
            throw std::invalid_argument("Unexpected end of input");
        }

        *charactersRead = endOfSymbol - pos;

        return source.substr(pos, *charactersRead);
    }

    /**
     * Read past given character, consuming any preceding whitespace.
     */
    void consumeChar(const std::string& str, char c, size_t& pos) {
        consumeWhiteSpace(str, pos);
        if (pos >= str.length()) {
            throw std::invalid_argument("Unexpected end of input");
        }
        if (str[pos] != c) {
            std::stringstream error;
            error << "Expected: \'" << c << "\', got: " << str[pos];
            throw std::invalid_argument(error.str());
        }
        ++pos;
    }

    /**
     * Advance position in the string until first non-whitespace character.
     */
    void consumeWhiteSpace(const std::string& str, size_t& pos) {
        while (pos < str.length() && std::isspace(static_cast<unsigned char>(str[pos]))) {
            ++pos;
        }
    }

    virtual std::unique_ptr<RamDomain[]> readNextTuple() = 0;
};

class ReadStreamFactory {
public:
    virtual std::unique_ptr<ReadStream> getReader(
            const std::map<std::string, std::string>&, SymbolTable&, RecordTable&) = 0;
    virtual const std::string& getName() const = 0;
    virtual ~ReadStreamFactory() = default;
};

} /* namespace souffle */
