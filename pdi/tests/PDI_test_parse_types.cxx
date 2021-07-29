/*******************************************************************************
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
	
#ifdef BUILD_FORTRAN
	{"character",                   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND}}},
	{"type: character",             shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND}}},
	{"{type: character, kind: 0}",  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND}}},
	{"{type: character, kind: 256}", shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, 256}}},
	{"integer",                     shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND}}},
	{"type: integer",               shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND}}},
	{"{type: integer, kind: 0}",    shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND}}},
	{"{type: integer, kind: 256}",   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::SIGNED, 256}}},
	{"logical",                     shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND}}},
	{"type: logical",               shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND}}},
	{"{type: logical, kind: 0}",    shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND}}},
	{"{type: logical, kind: 256}",   shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, 256}}},
	{"real",                        shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND}}},
	{"type: real",                  shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND}}},
	{"{type: real, kind: 0}",       shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND}}},
	{"{type: real, kind: 256}",      shared_ptr<Datatype>{new Scalar_datatype{Scalar_kind::FLOAT, 256}}},
#endif // BUILD_FORTRAN
};

vector<param_pair> array_types {
	{
		"{size: 10, type: array, subtype: char}",
		shared_ptr<Datatype> {
			new Array_datatype {
				unique_ptr<Datatype> (new Scalar_datatype {
					Scalar_kind::UNSIGNED, sizeof(char)
				}),
				10
			}
		}
	},
	{
		"{type: array, size: 20, subsize: 15, start: 5, subtype: char}",
		shared_ptr<Datatype> {
			new Array_datatype {
				unique_ptr<Datatype> (new Scalar_datatype {
					Scalar_kind::UNSIGNED, sizeof(char)
				}),
				20,
				5,
				15
			}
		}
	},
	{
		"{size: 30, subsize: 15, type: array, subtype: char}",
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
		"{size: 40, start: 20, subsize: 10, type: array, subtype: char}",
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
		"{size: [10000, 10000], type: array, subtype: int}",
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
		"{size: [10000, 10000], start: [256, 128], subsize: [1000, 2000], type: array, subtype: int}",
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
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
						"my_char"
					},
					Record_datatype::Member{
						4,
						unique_ptr<Datatype> {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
						"my_int"
					}
				},
				8
			}
		}
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
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
						"my_char"
					},
					Record_datatype::Member{
						8,
						unique_ptr<Datatype> {
							new Array_datatype
							{
								unique_ptr<Datatype> {
									new Array_datatype
									{
										unique_ptr<Datatype>{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
										10
									}
								},
								10
							}
						},
						"my_array"
					}
				},
				808
			}
		}
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
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
						"my_char"
					},
					Record_datatype::Member{
						8,
						unique_ptr<Datatype> {
							new Array_datatype
							{
								unique_ptr<Datatype> {
									new Array_datatype
									{
										unique_ptr<Datatype>{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
										10,
										3,
										5
									}
								},
								10,
								2,
								6
							}
						},
						"my_array"
					}
				},
				808
			}
		}
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
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
						"my_char"
					},
					Record_datatype::Member{
						8,
						unique_ptr<Datatype> {
							new Record_datatype {
								vector<Record_datatype::Member> {
									Record_datatype::Member{
										0,
										unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
										"my_char"
									},
									Record_datatype::Member{
										8,
										unique_ptr<Datatype> {
											new Array_datatype
											{
												unique_ptr<Datatype> {
													new Array_datatype
													{
														unique_ptr<Datatype>{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
														10
													}
												},
												10
											}
										},
										"my_array"
									}
								},
								808
							}
						},
						"my_record"
					}
				},
				816
			}
		}
	}
};

vector<param_pair> struct_types {
	{
		"type: struct    \n"
		"members:        \n"
		"   - my_char: char\n"
		"   - my_int: int  \n",
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
						"my_char"
					},
					Record_datatype::Member{
						4,
						unique_ptr<Datatype> {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
						"my_int"
					}
				},
				8
			}
		}
	},
	{
		"type: struct            \n"
		"members:                \n"
		"   - my_char: char        \n"
		"   - my_array:            \n"
		"       type: array        \n"
		"       subtype: int64     \n"
		"       size: [10, 10]     \n",
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
						"my_char"
					},
					Record_datatype::Member{
						8,
						unique_ptr<Datatype> {
							new Array_datatype
							{
								unique_ptr<Datatype> {
									new Array_datatype
									{
										unique_ptr<Datatype>{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
										10
									}
								},
								10
							}
						},
						"my_array"
					}
				},
				808
			}
		}
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
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
						"my_char"
					},
					Record_datatype::Member{
						8,
						unique_ptr<Datatype> {
							new Array_datatype
							{
								unique_ptr<Datatype> {
									new Array_datatype
									{
										unique_ptr<Datatype>{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
										10,
										3,
										5
									}
								},
								10,
								2,
								6
							}
						},
						"my_array"
					}
				},
				808
			}
		}
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
		"             size: [10, 10]     \n"
		,
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
						"my_char"
					},
					Record_datatype::Member{
						8,
						unique_ptr<Datatype> {
							new Record_datatype {
								vector<Record_datatype::Member> {
									Record_datatype::Member{
										0,
										unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
										"my_char"
									},
									Record_datatype::Member{
										8,
										unique_ptr<Datatype> {
											new Array_datatype
											{
												unique_ptr<Datatype> {
													new Array_datatype
													{
														unique_ptr<Datatype>{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
														10
													}
												},
												10
											}
										},
										"my_array"
									}
								},
								808
							}
						},
						"my_record"
					}
				},
				816
			}
		}
	}
};

vector<param_pair> pointer_types {
	{
		"{type: pointer, subtype: int}", shared_ptr<Datatype>{
			new Pointer_datatype {
				unique_ptr<Datatype>{
					new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}
				}
			}
		}
	},
	{
		"{type: pointer, subtype: {type: pointer, subtype: int}}", shared_ptr<Datatype>{
			new Pointer_datatype {
				unique_ptr<Datatype>{
					new Pointer_datatype {
						unique_ptr<Datatype>{
							new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}
						}
					}
				}
			}
		}
	},
	{
		"{type: pointer, subtype: {type: array, subtype: int, size: 32}}", shared_ptr<Datatype>{
			new Pointer_datatype {
				unique_ptr<Datatype>{
					new Array_datatype {
						unique_ptr<Datatype>{
							new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}
						},
						32
					}
				}
			}
		}
	},
	{
		"{type: array, subtype: {type: pointer, subtype: int}, size: 32}", shared_ptr<Datatype>{
			new Array_datatype {
				unique_ptr<Datatype>{
					new Pointer_datatype {
						unique_ptr<Datatype>{
							new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}
						}
					}
				},
				32
			}
		}
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
		shared_ptr<Datatype> {
			new Pointer_datatype {
				unique_ptr<Datatype> {
					new Record_datatype {
						vector<Record_datatype::Member> {
							Record_datatype::Member{
								0,
								unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
								"my_char"
							},
							Record_datatype::Member{
								4,
								unique_ptr<Datatype> {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
								"my_int"
							}
						},
						8
					}
				}
			}
		}
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
		shared_ptr<Datatype> {
			new Record_datatype {
				vector<Record_datatype::Member> {
					Record_datatype::Member{
						0,
						unique_ptr<Datatype> {
							new Pointer_datatype {
								unique_ptr<Datatype> {
									new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)}
								}
							},
						},
						"my_ptr"
					},
					Record_datatype::Member{
						8,
						unique_ptr<Datatype> {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
						"my_int"
					}
				},
				16
			}
		}
	}
};

vector<string> invalid_data {
	"",
	"long",
	"{size: [10, 20], type: array}"
	"{size: [10, 20], type: char}",
	"{size: [10, 20], start: [30, 20, 20], type: array, subtype: char}",
	"{size: [10, 20], subsize: 10, type: array, subtype: char}",
	"{subsize: 10, start: [30, 20, 20], type: array, subtype: char}",
	"{sizes: [10, 20], type: array, subtype: char}",
	"{type: record, members: {my_char: {disp: 0, type: char}, my_int: {disp: 4, type: int} }}",
	"{type: record, buffersize: 8, members: {my_char: {type: char}, my_int: {disp: 4, type: int} }}"
};
INSTANTIATE_TEST_CASE_P(ScalarTypes, PositiveTypeParseTest, testing::ValuesIn(scalar_types));
INSTANTIATE_TEST_CASE_P(ArrayTypes, PositiveTypeParseTest, testing::ValuesIn(array_types));
INSTANTIATE_TEST_CASE_P(RecordTypes, PositiveTypeParseTest, testing::ValuesIn(record_types));
INSTANTIATE_TEST_CASE_P(StructTypes, PositiveTypeParseTest, testing::ValuesIn(struct_types));
INSTANTIATE_TEST_CASE_P(PointerTypes, PositiveTypeParseTest, testing::ValuesIn(pointer_types));

INSTANTIATE_TEST_CASE_P(, NegativeTypeParseTest, testing::ValuesIn(invalid_data));
