/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <spdlog/spdlog.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include <pdi/array_datatype.h>
#include <pdi/datatype.h>
#include <pdi/datatype_template.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

#include "mocks/context_mock.h"

using PDI::Array_datatype;
using PDI::Datatype;
using PDI::Datatype_template;
using PDI::Paraconf_wrapper;
using PDI::Record_datatype;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;

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
	MockContext context_mock;
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
	auto&& params = GetParam();
	auto&& parsed_datatype = Datatype_template::load(PC_parse_string(params.first.c_str()), spdlog::stdout_color_mt("console"))->evaluate(this->context_mock);
	ASSERT_TRUE(*parsed_datatype == *params.second) << "When parsing: \"" << params.first << "\"" << std::endl
	    << "Expected: \"" << params.second->debug_string() << "\"" << std::endl
	    << "Actual: \"" << parsed_datatype->debug_string() << "\"" << std::endl;
}


/*
 * Struct prepared for NegativeTypeParseTest.
 */
struct NegativeTypeParseTest : public ::testing::TestWithParam<string> {
	MockContext context_mock;
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
	ASSERT_THROW(Datatype_template::load(PC_parse_string(GetParam().c_str()), spdlog::stdout_color_mt("console"))->evaluate(this->context_mock), PDI::Error);
}

vector<param_pair> scalar_types {
	{"char",                    shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)}}},
	{"type: char",              shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)}}},
	{"{type: char, kind: 0}",   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)}}},
	{"int",                     shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}}},
	{"type: int",               shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}}},
	{"{type: int, kind: 0}",    shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}}},
	{"int8",                    shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 1}}},
	{"type: int8",              shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 1}}},
	{"{type: int8, kind: 0}",   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 1}}},
	{"int16",                   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 2}}},
	{"type: int16",             shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 2}}},
	{"{type: int16, kind: 0}",  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 2}}},
	{"int32",                   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 4}}},
	{"type: int32",             shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 4}}},
	{"{type: int32, kind: 0}",  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 4}}},
	{"int64",                   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 8}}},
	{"type: int64",             shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 8}}},
	{"{type: int64, kind: 0}",  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 8}}},
	{"float",                   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(float)}}},
	{"type: float",             shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(float)}}},
	{"{type: float, kind: 0}",  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(float)}}},
	{"double",                  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(double)}}},
	{"type: double",            shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(double)}}},
	{"{type: double, kind: 0}", shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(double)}}},
	
	{"character",                   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND}}},
	{"type: character",             shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND}}},
	{"{type: character, kind: 0}",  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND}}},
	{"{type: character, kind: 10}", shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, 10}}},
	{"integer",                     shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND}}},
	{"type: integer",               shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND}}},
	{"{type: integer, kind: 0}",    shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND}}},
	{"{type: integer, kind: 10}",   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 10}}},
	{"logical",                     shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND}}},
	{"type: logical",               shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND}}},
	{"{type: logical, kind: 0}",    shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND}}},
	{"{type: logical, kind: 10}",   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, 10}}},
	{"real",                        shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND}}},
	{"type: real",                  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND}}},
	{"{type: real, kind: 0}",       shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND}}},
	{"{type: real, kind: 10}",      shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, 10}}},
};

vector<param_pair> array_types {
	{
		"{size: 10, type: char}",
		shared_ptr<Datatype> {
			new Array_datatype {
				unique_ptr<Datatype> (new Scalar_datatype {
					Scalar_kind::UNSIGNED, sizeof(char)
				}),
				10
			}
		}
	},
	// TODO: FIX THE BUG WITH PARSING
	// {
	//  "{size: 20, start: 5, type: char}",
	//  shared_ptr<Datatype> {
	//      new Array_datatype {
	//          unique_ptr<Datatype> (new Scalar_datatype {
	//              Scalar_kind::SIGNED, sizeof(char)
	//          }),
	//          20,
	//          5,
	//          15
	//      }
	//  }
	// },
	{
		"{size: 30, subsize: 15, type: char}",
		shared_ptr<Datatype> {
			new Array_datatype {
				unique_ptr<Datatype> (new Scalar_datatype {
					Scalar_kind::UNSIGNED, sizeof(char)
				}),
				30,
				0,
				15
			}
		}
	},
	{
		"{size: 40, start: 20, subsize: 10, type: char}",
		shared_ptr<Datatype> {
			new Array_datatype {
				unique_ptr<Datatype> (new Scalar_datatype {
					Scalar_kind::UNSIGNED, sizeof(char)
				}),
				40,
				20,
				10
			}
		}
	},
	{
		"{sizes: [10000, 10000], type: int}",
		shared_ptr<Datatype> {
			new Array_datatype {
				unique_ptr<Datatype> (new Array_datatype {
					unique_ptr<Datatype>(new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}), 10000
				}),
				10000,
			}
		}
	},
	// TODO: FIX THE BUG WITH PARSING
	// {
	//  "{sizes: [10000, 10000], starts: [512, 512], type: int}",
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
		"{sizes: [10000, 10000], subsizes: [200, 400], type: int}",
		shared_ptr<Datatype> {
			new Array_datatype {
				unique_ptr<Datatype> (new Array_datatype {
					unique_ptr<Datatype>(new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}), 10000, 0, 400
				}),
				10000,
				0,
				200
			}
		}
	},
	{
		"{sizes: [10000, 10000], starts: [256, 128], subsizes: [1000, 2000], type: int}",
		shared_ptr<Datatype> {
			new Array_datatype {
				unique_ptr<Datatype> (new Array_datatype {
					unique_ptr<Datatype>(new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}), 10000, 128, 2000
				}),
				10000,
				256,
				1000
			}
		}
	}
};

vector<string> invalid_data {
	"",
	"long",
	"{type: char, kind: 1}",
	"{sizes: 10, type: char}",
	"{size: [10, 20], type: char}",
};
INSTANTIATE_TEST_CASE_P(ScalarTypes, PositiveTypeParseTest, testing::ValuesIn(scalar_types));
INSTANTIATE_TEST_CASE_P(ArrayTypes, PositiveTypeParseTest, testing::ValuesIn(array_types));

INSTANTIATE_TEST_CASE_P(, NegativeTypeParseTest, testing::ValuesIn(invalid_data));
