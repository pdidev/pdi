/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <fstream>

#include <pdi/testing.h>

using testing::_;
using testing::AllOf;
using testing::Eq;
using testing::HasSubstr;
using testing::StartsWith;
using testing::StrEq;

/** \file 
 * 
 * Integration test for the include feature
 */

struct IncludeTest: public ::PDI::PdiTest {};

// single string include and getting a type from included file
TEST_F(IncludeTest, TypeFromInclude)
{
	{
		std::ofstream("TypeFromInclude.yml") << R"==(
types:
  my_int: int
)==";
	}
	InitPdi(PC_parse_string(R"==(
include: TypeFromInclude.yml
logging: trace
metadata:
  array_size: my_int
data:
  array_data:
    type: array
    subtype: double
    size: $array_size
)=="));

	int constexpr array_size = 7;
	auto const array = make_a<std::array<double, array_size>>();

	PDI_multi_expose("", "array_size", &array_size, PDI_OUT, "array_data", array.data(), PDI_OUT, nullptr);
}

// getting a type from including file
TEST_F(IncludeTest, TypeFromIncluding)
{
	{
		std::ofstream("TypeFromIncluding.yml") << R"==(
metadata:
  my_metadata: my_int
)==";
	}
	InitPdi(PC_parse_string(R"==(
include: TypeFromIncluding.yml
logging: trace
types:
  my_int: int
)=="));
}

// same type defined twice
TEST_F(IncludeTest, DuplicateType)
{
	{
		std::ofstream("DuplicateType.yml") << R"==(
types:
  my_int: int
)==";
	}
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_TYPE), _));
	InitPdi(PC_parse_string(R"==(
include: DuplicateType.yml
logging: trace
types:
  my_int: int
)=="));
}

// string list include and getting a metadata from included file
TEST_F(IncludeTest, MetadataFromInclude)
{
	{
		std::ofstream("MetadataFromInclude.yml") << R"==(
types:
  my_int: int
metadata:
  array_size: my_int
)==";
	}
	InitPdi(PC_parse_string(R"==(
include:
- MetadataFromInclude.yml
logging: trace
data:
  array_data:
    type: array
    subtype: double
    size: $array_size
  invalid_array:
    type: array
    subtype: double
    size: $invalid_ref
)=="));

	int constexpr array_size = 7;
	auto const array = make_a<std::array<double, array_size>>();

	PDI_multi_expose("", "array_size", &array_size, PDI_OUT, "array_data", array.data(), PDI_OUT, nullptr);

	// Reference to a non-existing metadata should fail
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_VALUE), _));
	PDI_expose("invalid_array", array.data(), PDI_OUT);
}

// same data defined twice
TEST_F(IncludeTest, DuplicateData)
{
	{
		std::ofstream("DuplicateData.yml") << R"==(
metadata:
  my_metadata: my_int
)==";
	}
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_SPECTREE), _));
	InitPdi(PC_parse_string(R"==(
include: DuplicateData.yml
logging: trace
data:
  my_metadata: my_int
)=="));
}

// subtree include and getting a data from included file
TEST_F(IncludeTest, DataFromIncludeSubtree)
{
	{
		std::ofstream("DataFromIncludeSubtree.yml") << R"==(
pdi_subtree:
  metadata:
    array_size: int
  data:
    array_data:
      type: array
      subtype: double
      size: $array_size
    invalid_array:
      type: array
      subtype: double
      size: $invalid_ref
)==";
	}
	InitPdi(PC_parse_string(R"==(
include:
  file: DataFromIncludeSubtree.yml
  subtree: .pdi_subtree
logging: trace
)=="));

	int constexpr array_size = 7;
	auto const array = make_a<std::array<double, array_size>>();

	PDI_multi_expose("", "array_size", &array_size, PDI_OUT, "array_data", array.data(), PDI_OUT, nullptr);

	// Reference to a non-existing metadata should fail
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_VALUE), _));
	PDI_expose("invalid_array", array.data(), PDI_OUT);
}

// subtree include list and getting a data from included file
TEST_F(IncludeTest, DataFromInclude)
{
	{
		std::ofstream("DataFromInclude.yml") << R"==(
types:
pdi_subtree:
  metadata:
    array_size: int
  data:
    array_data:
      type: array
      subtype: double
      size: $array_size
    invalid_array:
      type: array
      subtype: double
      size: $invalid_ref
)==";
	}
	InitPdi(PC_parse_string(R"==(
include:
- file: DataFromInclude.yml
  subtree: .pdi_subtree
logging: trace
)=="));

	int constexpr array_size = 7;
	auto const array = make_a<std::array<double, array_size>>();

	PDI_multi_expose("", "array_size", &array_size, PDI_OUT, "array_data", array.data(), PDI_OUT, nullptr);

	// Reference to a non-existing metadata should fail
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_VALUE), _));
	PDI_expose("invalid_array", array.data(), PDI_OUT);
}

// missing included file
TEST_F(IncludeTest, MissingFile)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_SYSTEM), _));
	InitPdi(PC_parse_string(R"==(
include:
- file: invalid_include.yml
  subtree: .pdi_subtree
logging: trace
)=="));
}

// missing subtree in included file
TEST_F(IncludeTest, MissingSubtree)
{
	{
		std::ofstream("MissingSubtree.yml") << R"==(
types:
pdi_subtree:
  metadata:
    array_size: int
  data:
    array_data:
      type: array
      subtype: double
      size: $array_size
)==";
	}
	InitPdi(PC_parse_string(R"==(
include:
- file: MissingSubtree.yml
  subtree: .pdi_subtree
logging: trace
)=="));

	int constexpr array_size = 7;
	auto const array = make_a<std::array<double, array_size>>();
	PDI_multi_expose("", "array_size", &array_size, PDI_OUT, "array_data", array.data(), PDI_OUT, nullptr);

	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_SYSTEM), _));
	InitPdi(PC_parse_string(R"==(
include:
- file: MissingSubtree.yml
  subtree: .invalid_subtree
logging: trace
)=="));
}

// recursive include is an error
TEST_F(IncludeTest, RecursiveInclude)
{
	{
		std::ofstream("RecursiveInclude.yml") << R"==(
include: RecursiveInclude.yml
)==";
	}
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_SPECTREE), _));
	InitPdi(PC_parse_string(R"==(
include: RecursiveInclude.yml
logging: trace
)=="));
}

// diamond include is OK
TEST_F(IncludeTest, DiamondInclude)
{
	{
		std::ofstream("DiamondInclude1.yml") << R"==(
types:
  my_int: int
)==";
		std::ofstream("DiamondInclude2.yml") << R"==(
include: DiamondInclude1.yml
)==";
		std::ofstream("DiamondInclude3.yml") << R"==(
include: DiamondInclude1.yml
)==";
	}
	InitPdi(PC_parse_string(R"==(
include: 
  - DiamondInclude2.yml
  - DiamondInclude3.yml
logging: trace
metadata:
  array_size: my_int
)=="));

	int constexpr array_size = 7;
	PDI_expose("array_size", &array_size, PDI_OUT);
}
