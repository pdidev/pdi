/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include "config.h"

#include <map>
#include <memory>
#include <sstream>
#include <utility>
#include <vector>

#include <gtest/gtest.h>

#include <pdi/array_datatype.h>
#include <pdi/datatype.h>
#include <pdi/datatype_template.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>
#include <pdi/tuple_datatype.h>

#include "global_context.h"


using PDI::Array_datatype;
using PDI::Datatype;
using PDI::Datatype_template;
using PDI::Global_context;
using PDI::Paraconf_wrapper;
using PDI::Pointer_datatype;
using PDI::Record_datatype;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::Tuple_datatype;

using std::map;
using std::pair;
using std::string;
using std::shared_ptr;
using std::unique_ptr;
using std::vector;

using param_pair = pair<string, shared_ptr<Datatype>>;

/*
 * Struct prepared for PositiveTypeParseTest.
 */
struct PositiveTypeParseTest : public ::testing::TestWithParam<param_pair> {
	PC_tree_t conf = PC_parse_string("logging: trace");
	Paraconf_wrapper _;
};

/*
 * Name:                PositiveTypeParseTest.parse
 *
 * Tested functions:    PDI::Datatype_template::load()
 *                      PDI::Datatype_template::evaluate(Context&)
 *
 * Description:         Checks if correct type is parsed from tree.
 */
TEST_P(PositiveTypeParseTest, parse)
{
	Global_context g_context {this->conf};
	auto&& params = GetParam();
	auto&& parsed_datatype = g_context.datatype(PC_parse_string(params.first.c_str()))->evaluate(g_context);
	ASSERT_TRUE(*parsed_datatype == *params.second) << "When parsing: \"" << params.first << "\"" << std::endl
	    << "Expected: \"" << params.second->debug_string() << "\"" << std::endl
	    << "Actual: \"" << parsed_datatype->debug_string() << "\"" << std::endl;
}


/*
 * Struct prepared for NegativeTypeParseTest.
 */
struct NegativeTypeParseTest : public ::testing::TestWithParam<string> {
	PC_tree_t conf = PC_parse_string("");
	Paraconf_wrapper _;
};


/*
 * Name:                NegativeTypeParseTest.parse
 *
 * Tested functions:    PDI::Datatype_template::load()
 *                      PDI::Datatype_template::evaluate(Context&)
 *
 * Description:         Checks if error is thrown when given invalid data.
 */
TEST_P(NegativeTypeParseTest, parse)
{
	Global_context g_context {this->conf};
	ASSERT_THROW(g_context.datatype(PC_parse_string(GetParam().c_str())), PDI::Error);
}

vector<param_pair> scalar_types {
	{"char",                                   Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
	{"type: char",                             Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
	{"{type: char, kind: 0}",                  Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
	{"int",                                    Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))},
	{"type: int",                              Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))},
	{"{type: int, kind: 0}",                   Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))},
	{"short",                                  Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(short))},
	{"type: short",                            Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(short))},
	{"{type: short, kind: 0}",                 Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(short))},
	{"unsigned short",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned short))},
	{"type: unsigned short",                   Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned short))},
	{"{type: unsigned short, kind: 0}",        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned short))},
	{"int8",                                   Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"type: int8",                             Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"{type: int8, kind: 0}",                  Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"int16",                                  Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"type: int16",                            Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"{type: int16, kind: 0}",                 Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"int32",                                  Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"type: int32",                            Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"{type: int32, kind: 0}",                 Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"int64",                                  Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"type: int64",                            Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"{type: int64, kind: 0}",                 Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"int8_t",                                 Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"type: int8_t",                           Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"{type: int8_t, kind: 0}",                Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"int16_t",                                Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"type: int16_t",                          Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"{type: int16_t, kind: 0}",               Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"int32_t",                                Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"type: int32_t",                          Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"{type: int32_t, kind: 0}",               Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"int64_t",                                Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"type: int64_t",                          Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"{type: int64_t, kind: 0}",               Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"uint8",                                Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"type: uint8",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"{type: uint8, kind: 0}",               Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"uint16",                               Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"type: uint16",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"{type: uint16, kind: 0}",              Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"uint32",                               Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"type: uint32",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"{type: uint32, kind: 0}",              Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"uint64",                               Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"type: uint64",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"{type: uint64, kind: 0}",              Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"uint8_t",                                Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"type: uint8_t",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"{type: uint8_t, kind: 0}",               Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"uint16_t",                               Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"type: uint16_t",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"{type: uint16_t, kind: 0}",              Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"uint32_t",                               Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"type: uint32_t",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"{type: uint32_t, kind: 0}",              Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"uint64_t",                               Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"type: uint64_t",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"{type: uint64_t, kind: 0}",              Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"int_least8",                           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"type: int_least8",                     Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"{type: int_least8, kind: 0}",          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"int_least16",                          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"type: int_least16",                    Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"{type: int_least16, kind: 0}",         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"int_least32",                          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"type: int_least32",                    Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"{type: int_least32, kind: 0}",         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"int_least64",                          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"type: int_least64",                    Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"{type: int_least64, kind: 0}",         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"int_least8_t",                           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"type: int_least8_t",                     Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"{type: int_least8_t, kind: 0}",          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"int_least16_t",                          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"type: int_least16_t",                    Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"{type: int_least16_t, kind: 0}",         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"int_least32_t",                          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"type: int_least32_t",                    Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"{type: int_least32_t, kind: 0}",         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"int_least64_t",                          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"type: int_least64_t",                    Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"{type: int_least64_t, kind: 0}",         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"uint_least8",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"type: uint_least8",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"{type: uint_least8, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"uint_least16",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"type: uint_least16",                   Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"{type: uint_least16, kind: 0}",        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"uint_least32",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"type: uint_least32",                   Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"{type: uint_least32, kind: 0}",        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"uint_least64",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"type: uint_least64",                   Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"{type: uint_least64, kind: 0}",        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"uint_least8_t",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"type: uint_least8_t",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"{type: uint_least8_t, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"uint_least16_t",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"type: uint_least16_t",                   Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"{type: uint_least16_t, kind: 0}",        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"uint_least32_t",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"type: uint_least32_t",                   Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"{type: uint_least32_t, kind: 0}",        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"uint_least64_t",                         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"type: uint_least64_t",                   Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"{type: uint_least64_t, kind: 0}",        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"int_fast8",                            Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"type: int_fast8",                      Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"{type: int_fast8, kind: 0}",           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"int_fast16",                           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"type: int_fast16",                     Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"{type: int_fast16, kind: 0}",          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"int_fast32",                           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"type: int_fast32",                     Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"{type: int_fast32, kind: 0}",          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"int_fast64",                           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"type: int_fast64",                     Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"{type: int_fast64, kind: 0}",          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"int_fast8_t",                            Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"type: int_fast8_t",                      Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"{type: int_fast8_t, kind: 0}",           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"int_fast16_t",                           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"type: int_fast16_t",                     Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"{type: int_fast16_t, kind: 0}",          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"int_fast32_t",                           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"type: int_fast32_t",                     Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"{type: int_fast32_t, kind: 0}",          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"int_fast64_t",                           Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"type: int_fast64_t",                     Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"{type: int_fast64_t, kind: 0}",          Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"uint_fast8",                           Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"type: uint_fast8",                     Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"{type: uint_fast8, kind: 0}",          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"uint_fast16",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"type: uint_fast16",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"{type: uint_fast16, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"uint_fast32",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"type: uint_fast32",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"{type: uint_fast32, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"uint_fast64",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"type: uint_fast64",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"{type: uint_fast64, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"uint_fast8_t",                           Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"type: uint_fast8_t",                     Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"{type: uint_fast8_t, kind: 0}",          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"uint_fast16_t",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"type: uint_fast16_t",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"{type: uint_fast16_t, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"uint_fast32_t",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"type: uint_fast32_t",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"{type: uint_fast32_t, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"uint_fast64_t",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"type: uint_fast64_t",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"{type: uint_fast64_t, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"intmax",                               Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"type: intmax",                         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"{type: intmax, kind: 0}",              Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"intmax_t",                               Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"type: intmax_t",                         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"{type: intmax_t, kind: 0}",              Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"uintmax",                              Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"type: uintmax",                        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"{type: uintmax, kind: 0}",             Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"uintmax_t",                              Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"type: uintmax_t",                        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"{type: uintmax_t, kind: 0}",             Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"intptr",                               Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"type: intptr",                         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"{type: intptr, kind: 0}",              Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"intptr_t",                               Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"type: intptr_t",                         Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"{type: intptr_t, kind: 0}",              Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"uintptr",                              Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"type: uintptr",                        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"{type: uintptr, kind: 0}",             Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"uintptr_t",                              Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"type: uintptr_t",                        Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"{type: uintptr_t, kind: 0}",             Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"long",                                   Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long))},
	{"type: long",                             Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long))},
	{"{type: long, kind: 0}",                  Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long))},
	{"unsigned long",                          Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long))},
	{"type: unsigned long",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long))},
	{"{type: unsigned long, kind: 0}",         Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long))},
	{"long long",                              Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long long))},
	{"type: long long",                        Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long long))},
	{"{type: long long, kind: 0}",             Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long long))},
	{"unsigned long long",                     Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long long))},
	{"type: unsigned long long",               Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long long))},
	{"{type: unsigned long long, kind: 0}",    Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long long))},
	{"float",                                  Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(float))},
	{"type: float",                            Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(float))},
	{"{type: float, kind: 0}",                 Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(float))},
	{"double",                                 Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(double))},
	{"type: double",                           Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(double))},
	{"{type: double, kind: 0}",                Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(double))},
	{"size_t",                                 Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(size_t))},
	{"type: size_t",                           Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(size_t))},
	{"{type: size_t, kind: 0}",                Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(size_t))},
	{"ptrdiff_t",                              Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(ptrdiff_t))},
	{"type: ptrdiff_t",                        Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(ptrdiff_t))},
	{"{type: ptrdiff_t, kind: 0}",             Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(ptrdiff_t))},
	{"byte",                                   Scalar_datatype::make(Scalar_kind::UNKNOWN, 1)},
	{"type: byte",                             Scalar_datatype::make(Scalar_kind::UNKNOWN, 1)},
	{"{type: byte, kind: 0}",                  Scalar_datatype::make(Scalar_kind::UNKNOWN, 1)},

	
#ifdef BUILD_FORTRAN
	{"character",                    Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND)},
	{"type: character",              Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND)},
	{"{type: character, kind: 0}",   Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND)},
	{"{type: character, kind: 256}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 256)},
	{"integer",                      Scalar_datatype::make(Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND)},
	{"type: integer",                Scalar_datatype::make(Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND)},
	{"{type: integer, kind: 0}",     Scalar_datatype::make(Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND)},
	{"{type: integer, kind: 256}",   Scalar_datatype::make(Scalar_kind::SIGNED, 256)},
	{"logical",                      Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND)},
	{"type: logical",                Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND)},
	{"{type: logical, kind: 0}",     Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND)},
	{"{type: logical, kind: 256}",   Scalar_datatype::make(Scalar_kind::UNSIGNED, 256)},
	{"real",                         Scalar_datatype::make(Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND)},
	{"type: real",                   Scalar_datatype::make(Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND)},
	{"{type: real, kind: 0}",        Scalar_datatype::make(Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND)},
	{"{type: real, kind: 256}",      Scalar_datatype::make(Scalar_kind::FLOAT, 256)},
#endif // BUILD_FORTRAN
};

vector<param_pair> array_types {
	{
		"{size: 10, type: array, subtype: char}",
		Array_datatype::make(
			Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
			10
		)
	},
	{
		"{type: array, size: 20, subsize: 15, start: 5, subtype: char}",
		Array_datatype::make(
			Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
			20,
			5,
			15
		)
	},
	{
		"{size: 30, subsize: 15, type: array, subtype: char}",
		Array_datatype::make(
			Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
			30,
			0,
			15
		)
	},
	{
		"{size: 40, start: 20, subsize: 10, type: array, subtype: char}",
		Array_datatype::make(
			Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
			40,
			20,
			10
		)
	},
	{
		"{size: [10000, 10000], type: array, subtype: int}",
		Array_datatype::make(
			Array_datatype::make(
				Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
				10000
			),
			10000
		)
	},
	// TODO: FIX THE BUG WITH PARSING
	// {
	//  "{size: [10000, 10000], start: [512, 512], type: array, subtype: int}",
	//  shared_ptr<Datatype> {
	//      new Array_datatype {
	//          unique_ptr<Datatype> (new Array_datatype {
	//              unique_ptr<Datatype>(new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}), 10000, 512, 10000 - 512
	//          }),
	//          10000,
	//          512,
	//          10000 - 512
	//      }
	//  }
	// },
	{
		"{size: [10000, 10000], subsize: [200, 400], type: array, subtype: int}",
		Array_datatype::make(
			Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10000, 0, 400),
			10000,
			0,
			200
		)
	},
	{
		"{size: [10000, 10000], start: [256, 128], subsize: [1000, 2000], type: array, subtype: int}",
		Array_datatype::make(
			Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10000, 128, 2000),
			10000,
			256,
			1000
		)
	}
};

vector<param_pair> record_types {
	{
		"type: record    \n"
		"buffersize: 8   \n"
		"members:        \n"
		"   my_char:     \n"
		"     disp: 0    \n"
		"     type: char \n"
		"   my_int:      \n"
		"     disp: 4    \n"
		"     type: int  \n",
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					4,
					Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
					"my_int"
				}
			},
			8
		)
	},
	{
		"type: record            \n"
		"buffersize: 808         \n"
		"members:                \n"
		"   my_char:             \n"
		"     disp: 0            \n"
		"     type: char         \n"
		"   my_array:            \n"
		"     disp: 8            \n"
		"     type: array        \n"
		"     subtype: int64\n"
		"     size: [10, 10]     \n",
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					8,
					Array_datatype::make(
						Array_datatype::make(
							Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
							10
						),
						10
					),
					"my_array"
				}
			},
			808
		)
	},
	{
		"type: record            \n"
		"buffersize: 808         \n"
		"members:                \n"
		"   my_char:             \n"
		"     disp: 0            \n"
		"     type: char         \n"
		"   my_array:            \n"
		"     disp: 8            \n"
		"     type: array        \n"
		"     subtype: int64     \n"
		"     size: [10, 10]     \n"
		"     start: [2, 3]      \n"
		"     subsize: [6, 5]    \n",
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					8,
					Array_datatype::make(
						Array_datatype::make(
							Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
							10,
							3,
							5
						),
						10,
						2,
						6
					),
					"my_array"
				}
			},
			808
		)
	},
	{
		"type: record                \n"
		"buffersize: 816             \n"
		"members:                    \n"
		"   my_char:                 \n"
		"     disp: 0                \n"
		"     type: char             \n"
		"   my_record:               \n"
		"     disp: 8                \n"
		"     type: record           \n"
		"     buffersize: 808        \n"
		"     members:               \n"
		"       my_char:             \n"
		"         disp: 0            \n"
		"         type: char         \n"
		"       my_array:            \n"
		"         disp: 8            \n"
		"         type: array        \n"
		"         subtype: int64\n"
		"         size: [10, 10]     \n"
		,
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					8,
					Record_datatype::make(
						vector<Record_datatype::Member> {
							Record_datatype::Member{
								0,
								Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
								"my_char"
							},
							Record_datatype::Member{
								8,
								Array_datatype::make(
									Array_datatype::make(
										Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
										10
									),
									10
								),
								"my_array"
							}
						},
						808
					),
					"my_record"
				}
			},
			816
		)
	}
};

vector<param_pair> struct_types {
	{
		"type: struct    \n"
		"members:        \n"
		"   - my_char: char\n"
		"   - my_int: int  \n",
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					4,
					Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
					"my_int"
				}
			},
			8
		)
	},
	{
		"type: struct            \n"
		"members:                \n"
		"   - my_char: char        \n"
		"   - my_array:            \n"
		"       type: array        \n"
		"       subtype: int64     \n"
		"       size: [10, 10]     \n",
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					8,
					Array_datatype::make(
						Array_datatype::make(
							Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
							10
						),
						10
					),
					"my_array"
				}
			},
			808
		)
	},
	{
		"type: struct              \n"
		"members:                  \n"
		"   - my_char: char        \n"
		"   - my_array:            \n"
		"       type: array        \n"
		"       subtype: int64     \n"
		"       size: [10, 10]     \n"
		"       start: [2, 3]      \n"
		"       subsize: [6, 5]    \n",
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					8,
					Array_datatype::make(
						Array_datatype::make(
							Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
							10,
							3,
							5
						),
						10,
						2,
						6
					),
					"my_array"
				}
			},
			808
		)
	},
	{
		"type: struct                \n"
		"members:                    \n"
		"   - my_char: char            \n"
		"   - my_record:               \n"
		"       type: struct           \n"
		"       members:               \n"
		"         - my_char:             \n"
		"             type: char         \n"
		"         - my_array:            \n"
		"             type: array        \n"
		"             subtype: int64     \n"
		"             size: [10, 10]     \n",
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					8,
					Record_datatype::make(
						vector<Record_datatype::Member> {
							Record_datatype::Member{
								0,
								Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
								"my_char"
							},
							Record_datatype::Member{
								8,
								Array_datatype::make(
									Array_datatype::make(
										Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
										10
									),
									10
								),
								"my_array"
							}
						},
						808
					),
					"my_record"
				}
			},
			816
		)
	}
};

vector<param_pair> pointer_types {
	{
		"{type: pointer, subtype: int}", 
		Pointer_datatype::make(
			Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))
		)
	},
	{
		"{type: pointer, subtype: {type: pointer, subtype: int}}",
		Pointer_datatype::make(
			Pointer_datatype::make(
				Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))
			)
		)
	},
	{
		"{type: pointer, subtype: {type: array, subtype: int, size: 32}}",
		Pointer_datatype::make(
			Array_datatype::make(
				Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
				32
			)
		)
	},
	{
		"{type: array, subtype: {type: pointer, subtype: int}, size: 32}",
		Array_datatype::make(
			Pointer_datatype::make(
				Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))
			),
			32
		)
	},
	{
		"type: pointer     \n"
		"subtype:          \n"
		"  type: record    \n"
		"  buffersize: 8   \n"
		"  members:        \n"
		"     my_char:     \n"
		"       disp: 0    \n"
		"       type: char \n"
		"     my_int:      \n"
		"       disp: 4    \n"
		"       type: int  \n",
		Pointer_datatype::make(
			Record_datatype::make(
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
						"my_char"
					},
					Record_datatype::Member{
						4,
						Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
						"my_int"
					}
				},
				8
			)
		)
	},
	{
		"type: record       \n"
		"buffersize: 16     \n"
		"members:           \n"
		"   my_ptr:         \n"
		"     disp: 0       \n"
		"     type: pointer \n"
		"     subtype: char \n"
		"   my_int:         \n"
		"     disp: 8       \n"
		"     type: int     \n",
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Pointer_datatype::make(
						Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))
					),
					"my_ptr"
				},
				Record_datatype::Member{
					8,
					Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
					"my_int"
				}
			},
			16
		)
	}
};

vector<param_pair> tuple_types {
	{
		"type: tuple    \n"
		"elements:        \n"
		"   - char\n"
		"   - int  \n",
		Tuple_datatype::make(
			vector<Tuple_datatype::Element> {
				Tuple_datatype::Element{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))
				},
				Tuple_datatype::Element{
					4,
					Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))
				}
			},
			8
		)
	},
	{
		"type: tuple       \n"
		"buffersize: 13    \n"
		"elements:         \n"
		"   - type: char   \n"
		"     disp: 0      \n"
		"   - type: int    \n"
		"     disp: 1      \n"
		"   - type: double \n"
		"     disp: 5      \n",
		Tuple_datatype::make(
			vector<Tuple_datatype::Element> {
				Tuple_datatype::Element{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))
				},
				Tuple_datatype::Element{
					1,
					Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))
				},
				Tuple_datatype::Element{
					5,
					Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(double))
				}
			},
			13
		)
	},
	{
		"type: tuple             \n"
		"elements:               \n"
		"   - char               \n"
		"   - type: array        \n"
		"     subtype: int64     \n"
		"     size: [10, 10]     \n",
		Tuple_datatype::make(
			vector<Tuple_datatype::Element> {
				Tuple_datatype::Element{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))
				},
				Tuple_datatype::Element{
					8,
					Array_datatype::make(
						Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10),
						10
					)
				}
			},
			808
		)
	},
	{
		"type: tuple             \n"
		"elements:               \n"
		"   - char               \n"
		"   - type: array        \n"
		"     subtype: int64     \n"
		"     size: [10, 10]     \n"
		"     start: [2, 3]      \n"
		"     subsize: [6, 5]    \n",
		Tuple_datatype::make(
			vector<Tuple_datatype::Element> {
				Tuple_datatype::Element{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))
				},
				Tuple_datatype::Element{
					8,
					Array_datatype::make(
						Array_datatype::make(
							Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
							10,
							3,
							5
						),
						10,
						2,
						6
					)
				}
			},
			808
		)
	},
	{
		"type: tuple                   \n"
		"elements:                     \n"
		"   - char                     \n"
		"   - type: struct             \n"
		"     members:                 \n"
		"       - my_char:             \n"
		"           type: char         \n"
		"       - my_array:            \n"
		"           type: array        \n"
		"           subtype: int64     \n"
		"           size: [10, 10]     \n"
		,
		Tuple_datatype::make(
			vector<Tuple_datatype::Element> {
				Tuple_datatype::Element{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))
				},
				Tuple_datatype::Element{
					8,
					Record_datatype::make(
						vector<Record_datatype::Member> {
							Record_datatype::Member{
								0,
								Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
								"my_char"
							},
							Record_datatype::Member{
								8,
								Array_datatype::make(
									Array_datatype::make(
										Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
										10
									),
									10
								),
								"my_array"
							}
						},
						808
					)
				}
			},
			816
		)
	},
	{
		"type: tuple                     \n"
		"elements:                       \n"
		"   - char                       \n"
		"   - type: tuple              \n"
		"     elements:                \n"
		"       - type: char           \n"
		"       - type: array          \n"
		"         subtype: int64       \n"
		"         size: [10, 10]       \n"
		,
		Tuple_datatype::make(
			vector<Tuple_datatype::Element> {
				Tuple_datatype::Element{
					0,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))
				},
				Tuple_datatype::Element{
					8,
					Tuple_datatype::make(
						vector<Tuple_datatype::Element> {
							Tuple_datatype::Element{
								0,
								Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))
							},
							Tuple_datatype::Element{
								8,
								Array_datatype::make(
									Array_datatype::make(
										Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
										10
									),
									10
								)
							}
						},
						808
					)
				}
			},
			816
		)
	}
};

vector<string> invalid_data {
	"",
	"{size: [10, 20], type: array}"
	"{size: [10, 20], type: char}",
	"{size: [10, 20], start: [30, 20, 20], type: array, subtype: char}",
	"{size: [10, 20], subsize: 10, type: array, subtype: char}",
	"{subsize: 10, start: [30, 20, 20], type: array, subtype: char}",
	"{sizes: [10, 20], type: array, subtype: char}",
	"{type: record, members: {my_char: {disp: 0, type: char}, my_int: {disp: 4, type: int} }}",
	"{type: record, buffersize: 8, members: {my_char: {type: char}, my_int: {disp: 4, type: int} }}",
	"{type: tuple, buffersize: 13, elements: [char, int, double]}",
	"{type: tuple, elements: [{type: char, disp: 0}, {type: int, disp: 4}, {type: double, disp: 8}]}",
	"{type: tuple, elements: [{type: char, disp: 0}, {type: int}, {type: double, disp: 8}]}"
};
INSTANTIATE_TEST_CASE_P(ScalarTypes, PositiveTypeParseTest, testing::ValuesIn(scalar_types));
INSTANTIATE_TEST_CASE_P(ArrayTypes, PositiveTypeParseTest, testing::ValuesIn(array_types));
INSTANTIATE_TEST_CASE_P(RecordTypes, PositiveTypeParseTest, testing::ValuesIn(record_types));
INSTANTIATE_TEST_CASE_P(StructTypes, PositiveTypeParseTest, testing::ValuesIn(struct_types));
INSTANTIATE_TEST_CASE_P(TupleTypes, PositiveTypeParseTest, testing::ValuesIn(tuple_types));
INSTANTIATE_TEST_CASE_P(PointerTypes, PositiveTypeParseTest, testing::ValuesIn(pointer_types));

INSTANTIATE_TEST_CASE_P(, NegativeTypeParseTest, testing::ValuesIn(invalid_data));
