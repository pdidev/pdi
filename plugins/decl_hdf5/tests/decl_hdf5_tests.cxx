/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <filesystem>
#include <iostream>
#include <numeric>
#include <ranges>

#include <pdi/testing.h>

using PDI::make_random;
using PDI::random_init;
using testing::Eq;
using testing::StartsWith;
using testing::StrEq;

struct Simple_record {
	int x;
	int y;
	int z;
	bool operator== (const Simple_record&) const = default;

	void init_from(std::uniform_random_bit_generator auto& gen)
	{
		random_init(gen, x);
		random_init(gen, y);
		random_init(gen, z);
	}
};

std::ostream& operator<< (std::ostream& out, Simple_record const & r)
{
	return out << "Simple_record(x=" << r.x << ", y=" << r.y << ", z=" << r.z << ")";
}

struct Complex_record {
	int id;
	Simple_record value[4];
	bool operator== (const Complex_record&) const = default;

	void init_from(std::uniform_random_bit_generator auto& gen)
	{
		random_init(gen, id);
		random_init(gen, value);
	}
};

std::ostream& operator<< (std::ostream& out, Complex_record const & r)
{
	return out << "Complex_record(id=" << r.id << ", value=" << r.value << ")";
}

class DeclHdf5: public ::PDI::PdiTest
{};

/* Metatadata use in filename expression & write on data
 */
TEST_F(DeclHdf5, FilenameFromMetaList)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { meta_var: int }
data: { test_var: double }
plugins:
  decl_hdf5:
    file: "file${meta_var}.h5"
    write: [ test_var ]
)=="));

	int const meta_var = 1;
	PDI_expose("meta_var", &meta_var, PDI_OUT);

	auto const test_var = make_a<double>();
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_TRUE(std::filesystem::exists("file1.h5"));
}

/* Metatadata use in filename expression & write on data (from mapping)
 */
TEST_F(DeclHdf5, FilenameFromMetaMapping)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { meta_var: int }
data: { test_var: double }
plugins:
  decl_hdf5:
    file: file${meta_var}.h5
    write: { test_var: ~ }
)=="));

	int const meta_var = 2;
	PDI_expose("meta_var", &meta_var, PDI_OUT);

	auto const test_var = make_a<double>();
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_TRUE(std::filesystem::exists("file2.h5"));
}

/* Metatadata use in filename expression & write on event
 */
TEST_F(DeclHdf5, FilenameFromMetaEvent)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { meta_var: int }
data: { test_var: double }
plugins:
  decl_hdf5:
    file: file${meta_var}.h5
    on_event: "event"
    write: { test_var: ~ }
)=="));

	int const meta_var = 3;
	PDI_expose("meta_var", &meta_var, PDI_OUT);

	auto const test_var = make_a<double>();
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_FALSE(std::filesystem::exists("file3.h5"));

	PDI_multi_expose("wrong_event", "test_var", &test_var, PDI_OUT, nullptr);
	EXPECT_FALSE(std::filesystem::exists("file3.h5"));

	PDI_multi_expose("event", "test_var", &test_var, PDI_OUT, nullptr);
	EXPECT_TRUE(std::filesystem::exists("file3.h5"));
}

/* Write then read complex structures (record of array of record)
 */
TEST_F(DeclHdf5, RecordOfArrayOfRecord)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  step: int
data:
  complex_record:
    type: struct
    members:
      - id: int
      - value:
          type: array
          size: 4
          subtype:
            type: struct
            members:
              - x: int
              - y: int
              - z: int
plugins:
  decl_hdf5:
    - file: decl_hdf5_test_outer_record.h5
      write: [complex_record]
      when: $step=0
    - file: decl_hdf5_test_outer_record.h5
      read: [complex_record]
      when: $step=1
)=="));

	// write to file
	int step = 0;
	PDI_expose("step", &step, PDI_OUT);

	auto const complex_record = make_a<Complex_record>();
	PDI_expose("complex_record", &complex_record, PDI_OUT);

	// read from file
	step = 1;
	PDI_expose("step", &step, PDI_OUT);

	Complex_record complex_record_read;
	PDI_expose("complex_record", &complex_record_read, PDI_IN);
	EXPECT_EQ(complex_record, complex_record_read);
}

/* Write then read a selection of dataset of records
 */
TEST_F(DeclHdf5, ArrayOfRecordSelection)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  step: int
types:
  Complex_record:
    type: struct
    members:
      - id: int
      - value:
          type: array
          size: 4
          subtype:
            type: struct
            members:
              - x: int
              - y: int
              - z: int
data:
  array_of_record:
    type: array
    size: 4
    subtype: Complex_record
  array_of_record_read: Complex_record
plugins:
  decl_hdf5:
    - file: decl_hdf5_test_record_and_selection.h5
      datasets:
        array_of_record:
          type: array
          size: 2
          subtype: Complex_record
      write:
        array_of_record:
          dataset: array_of_record
          # select a subset of the data in memory to write
          memory_selection: { start: 1, size: 2 }
      read:
        array_of_record_read:
          dataset: array_of_record
          # select a subset of the data in file to read
          dataset_selection: { start: 1, size: 1 }
)=="));

	// write to file
	auto const array_of_record = make_a<std::array<Complex_record, 4>>();
	PDI_expose("array_of_record", array_of_record.data(), PDI_OUT);

	// load from file
	Complex_record array_of_record_read;
	PDI_expose("array_of_record_read", &array_of_record_read, PDI_IN);
	// we excluded 1 element on write and 1 on read, so we lost 2
	EXPECT_EQ(array_of_record[2], array_of_record_read);
}

/* attribute depend on group
 */
TEST_F(DeclHdf5, AttributesGroup)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  array_size: int
data:
  array_data: { size: $array_size, type: array, subtype: int }
  group_attr: float
  dset_attr: {type: array, subtype: int, size: 4}
  array_data_read: { size: $array_size, type: array, subtype: int }
  group_attr_read: float
  dset_attr_read: {type: array, subtype: int, size: 4}
  expr_attr: int
plugins:
  decl_hdf5:
  - file: decl_hdf5_test_attribute_group.h5
    datasets:
      data/array_data: { size: $array_size, type: array, subtype: int }
    write:
      array_data:
        dataset: data/array_data
        attributes:
          expr_attr: $array_size
      dset_attr:
        attribute: data/array_data#dset_attr_name
      group_attr:
        attribute: data#group_attr_name
    read:
      array_data_read:
        dataset: data/array_data
        attributes:
          expr_attr: $expr_attr
      dset_attr_read:
        attribute: data/array_data#dset_attr_name
      group_attr_read:
        attribute: data#group_attr_name
)=="));

	static constexpr int const array_size = 10;
	PDI_expose("array_size", &array_size, PDI_OUT);

	auto const array_data = make_a<std::array<int, array_size>>();
	PDI_expose("array_data", array_data.data(), PDI_OUT);

	auto const group_attr = make_a<float>();
	PDI_expose("group_attr", &group_attr, PDI_OUT);

	auto const dset_attr = make_a<std::array<int, 4>>();
	PDI_expose("dset_attr", dset_attr.data(), PDI_OUT);

	float group_attr_read = 0;
	PDI_expose("group_attr_read", &group_attr_read, PDI_IN);
	EXPECT_EQ(group_attr, group_attr_read);

	std::array<int, 4> dset_attr_read;
	PDI_expose("dset_attr_read", dset_attr_read.data(), PDI_IN);
	EXPECT_EQ(dset_attr, dset_attr_read);

	int expr_attr = 0;
	std::array<int, array_size> array_data_read;
	PDI_share("expr_attr", &expr_attr, PDI_IN);
	PDI_expose("array_data_read", array_data_read.data(), PDI_IN);
	EXPECT_EQ(array_data, array_data_read);
	PDI_reclaim("expr_attr");
	EXPECT_EQ(array_size, expr_attr);
}

/* attribute depend on data
 */
TEST_F(DeclHdf5, AttributesData)
{
	//PDI_write
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  array_size: int
data:
  array_data: { size: $array_size, type: array, subtype: int }
  dset_attr: {type: array, subtype: int, size: 4}
  size_attr: int
plugins:
  decl_hdf5:
  - file: decl_hdf5_test_attribute_data.h55
    on_event: "write"
    write: [array_data, array_data#dset_attr, array_data#size_attr]
  - file: decl_hdf5_test_attribute_data.h55
    on_event: "read"
    read: [array_data, array_data#dset_attr, array_data#size_attr]
)=="));

	static constexpr int const array_size = 10;
	PDI_expose("array_size", &array_size, PDI_OUT);

	auto const array_data = make_a<std::array<int, array_size>>();
	PDI_expose("array_data", array_data.data(), PDI_OUT);

	auto const dset_attr = make_a<std::array<int, 4>>();
	PDI_multi_expose(
		"write",
		"size_attr",
		&array_size,
		PDI_OUT,
		"array_data",
		array_data.data(),
		PDI_OUT,
		"dset_attr",
		dset_attr.data(),
		PDI_OUT,
		NULL
	);

	int size_attr = 0;
	std::array<int, 4> dset_attr_read;
	std::array<int, array_size> array_data_read;
	PDI_multi_expose(
		"read",
		"size_attr",
		&size_attr,
		PDI_IN,
		"array_data",
		array_data_read.data(),
		PDI_IN,
		"dset_attr",
		dset_attr_read.data(),
		PDI_IN,
		NULL
	);
	EXPECT_EQ(array_size, size_attr);
	EXPECT_EQ(dset_attr, dset_attr_read);
	EXPECT_EQ(array_data, array_data_read);
}

/* read dataset size before dataset itself
 */
TEST_F(DeclHdf5, SizeOf)
{
	//PDI_write
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  step: int
data:
  array_data_size: int64
  array_data: { size: 10, type: array, subtype: int }
  matrix_data_size: { size: 2, type: array, subtype: int64 }
  matrix_data: { size: [10, 10], type: array, subtype: float }
plugins:
  decl_hdf5:
  - file: decl_hdf5_test_read_dataset_size_before_dataset_itself.h5
    when: $step=0
    write: [array_data, matrix_data]
  - file: decl_hdf5_test_read_dataset_size_before_dataset_itself.h5
    when: $step
    read:
      array_data_size:
        size_of: array_data
      matrix_data_size:
        size_of: matrix_data
  - file: decl_hdf5_test_read_dataset_size_before_dataset_itself.h5
    on_event: "read_size"
    read:
      array_data_size:
        size_of: array_data
      matrix_data_size:
        size_of: matrix_data)=="));

	int step = 0;
	PDI_expose("step", &step, PDI_OUT);

	auto const array_data = make_a<std::array<int, 10>>();
	PDI_expose("array_data", array_data.data(), PDI_OUT);

	auto const matrix_data = make_a<std::array<int[10], 10>>();
	PDI_expose("matrix_data", matrix_data.data(), PDI_OUT);

	step = 1;
	PDI_expose("step", &step, PDI_OUT);

	long array_size = 0;
	PDI_expose("array_data_size", &array_size, PDI_IN);
	EXPECT_EQ(10, array_size);

	std::array<long, 2> matrix_size = {};
	PDI_expose("matrix_data_size", matrix_size.data(), PDI_IN);
	EXPECT_EQ(10, matrix_size[0]);
	EXPECT_EQ(10, matrix_size[1]);

	// now with event
	step = 2;
	PDI_expose("step", &step, PDI_OUT);

	long array_size_event = 0;
	std::array<long, 2> matrix_size_event;
	PDI_multi_expose("read_size", "array_data_size", &array_size_event, PDI_IN, "matrix_data_size", matrix_size_event.data(), PDI_IN, nullptr);
	EXPECT_EQ(10, array_size_event);
	EXPECT_EQ(10, matrix_size_event[0]);
	EXPECT_EQ(10, matrix_size_event[1]);
}

/* different dimension of data and dataset
 */
TEST_F(DeclHdf5, DifferentDim)
{
	//PDI_write
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  scalar_data: double
  array_data: { size: [8, 8], type: array, subtype: int }
plugins:
  decl_hdf5:
    file: decl_hdf5_test_different_dim_of_data_and_dataset.h5
    datasets:
      scalar_data: {type: array, subtype: double, size: 1}
      array_data: {type: array, subtype: int, size: [4, 4, 4]}
    write: [scalar_data, array_data]
)=="));

	auto const scalar_data = make_a<double>();
	PDI_expose("scalar_data", &scalar_data, PDI_OUT);

	auto const array_data = make_a<std::array<std::array<int, 8>, 8>>();
	PDI_expose("array_data", array_data.data(), PDI_OUT);

	FinalizePdi();

	//PDI_read
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  scalar_data: double
  array_data: { size: [8, 8], type: array, subtype: int }
plugins:
  decl_hdf5:
    file: decl_hdf5_test_different_dim_of_data_and_dataset.h5
    datasets:
      scalar_data: {type: array, subtype: double, size: 1}
      array_data: {type: array, subtype: int, size: [4, 4, 4]}
    read: [scalar_data, array_data]
)=="));

	double scalar_data_read = 0;
	PDI_expose("scalar_data", &scalar_data_read, PDI_IN);
	EXPECT_EQ(scalar_data, scalar_data_read);

	std::array<std::array<int, 8>, 8> array_data_read = {};
	PDI_expose("array_data", array_data_read.data(), PDI_IN);
	EXPECT_EQ(array_data, array_data_read);
}

/* Test of the various collision policies
 */
TEST_F(DeclHdf5, Collision)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  scalar_data: int
  array_data: { size: [4, 4], type: array, subtype: int }
plugins:
  decl_hdf5:
    - file: decl_hdf5_test_collision_policy.h5
      on_event: init
      write: [array_data]
    - file: decl_hdf5_test_collision_policy.h5
      collision_policy: skip
      on_event: skip
      write: [array_data]
    - file: decl_hdf5_test_collision_policy.h5
      collision_policy: write_into
      on_event: write_into
      write: [scalar_data, array_data]
    - file: decl_hdf5_test_collision_policy.h5
      collision_policy: replace
      on_event: replace
      write: [array_data]
    - file: decl_hdf5_test_collision_policy.h5
      collision_policy: write_into
      on_event: append
      write:
        scalar_data:
          collision_policy: error
    - file: decl_hdf5_test_collision_policy.h5
      collision_policy: error
      on_event: error
      write: [array_data]
    - file: decl_hdf5_test_collision_policy.h5
      on_event: read
      read: [array_data]
    - file: decl_hdf5_test_collision_policy.h5
      on_event: read_scalar
      read: [scalar_data]
)=="));

	// INIT => array_data = array_data_1
	auto const array_data_1 = make_a<std::array<std::array<int, 4>, 4>>();
	PDI_multi_expose("init", "array_data", array_data_1.data(), PDI_OUT, nullptr);

	// SKIP => array_data already present, do nothing
	auto const array_data_2 = make_a<std::array<std::array<int, 4>, 4>>();
	PDI_multi_expose("skip", "array_data", array_data_2.data(), PDI_OUT, nullptr);

	{ // check that array_data == array_data_pos
		std::array<std::array<int, 4>, 4> array_data_read;
		PDI_multi_expose("read", "array_data", array_data_read.data(), PDI_IN, nullptr);
		EXPECT_EQ(array_data_1, array_data_read);
	}

	// WRITE_INTO => scalar_data = scalar_data_val; array_data = array_data_3
	auto const scalar_data_val = make_a<int>();
	auto const array_data_3 = make_a<std::array<std::array<int, 4>, 4>>();
	PDI_multi_expose("write_into", "scalar_data", &scalar_data_val, PDI_OUT, "array_data", array_data_3.data(), PDI_OUT, nullptr);

	{ // check that scalar_data == scalar_data_val
		int scalar_data_read = 0;
		PDI_multi_expose("read_scalar", "scalar_data", &scalar_data_read, PDI_IN, nullptr);
		EXPECT_EQ(scalar_data_val, scalar_data_read);
	}

	{ // check that array_data == array_data_3
		std::array<std::array<int, 4>, 4> array_data_read;
		PDI_multi_expose("read", "array_data", array_data_read.data(), PDI_IN, nullptr);
		EXPECT_EQ(array_data_3, array_data_read);
	}

	// REPLACE => array_data = array_data_4; removes scalar_data
	auto const array_data_4 = make_a<std::array<std::array<int, 4>, 4>>();
	PDI_multi_expose("replace", "array_data", array_data_4.data(), PDI_OUT, nullptr);

	{ // check that array_data == array_data_4
		std::array<std::array<int, 4>, 4> array_data_read;
		PDI_multi_expose("read", "array_data", array_data_read.data(), PDI_IN, nullptr);
		EXPECT_EQ(array_data_4, array_data_read);
	}

	{ // check that scalar_data is missing
		int scalar_data_read = 0;
		EXPECT_CALL(
			*this,
			PdiError(
				Eq(PDI_ERR_SYSTEM),
				StartsWith("Error while triggering event `read_scalar': "
		                   "System_error: Cannot open `scalar_data' dataset object 'scalar_data' doesn't exist")
			)
		);
		PDI_multi_expose("read_scalar", "scalar_data", &scalar_data_read, PDI_IN, nullptr);
	}

	// APPEND => scalar_data = scalar_data_val
	PDI_multi_expose("append", "scalar_data", &scalar_data_val, PDI_OUT, nullptr);

	{ // check that scalar_data == scalar_data_val
		int scalar_data_read = 0;
		PDI_multi_expose("read_scalar", "scalar_data", &scalar_data_read, PDI_IN, nullptr);
		EXPECT_EQ(scalar_data_val, scalar_data_read);
	}

	{ // check that array_data == array_data_4
		std::array<std::array<int, 4>, 4> array_data_read;
		PDI_multi_expose("read", "array_data", array_data_read.data(), PDI_IN, nullptr);
		EXPECT_EQ(array_data_4, array_data_read);
	}

	// APPEND => error (scalar_data already exists)
	EXPECT_CALL(
		*this,
		PdiError(
			Eq(PDI_ERR_SYSTEM),
			StrEq("Error while triggering event `append': System_error: Dataset collision `scalar_data': Dataset already exists")
		)
	);
	auto const scalar_data_2 = make_a<int>();
	PDI_multi_expose("append", "scalar_data", &scalar_data_2, PDI_OUT, nullptr);

	// ERROR => error (file already exists)
	EXPECT_CALL(
		*this,
		PdiError(
			Eq(PDI_ERR_SYSTEM),
			StrEq("Error while triggering event `error': System_error: Filename collision `decl_hdf5_test_collision_policy.h5': File already exists")
		)
	);
	auto const array_data_5 = make_a<std::array<std::array<int, 4>, 4>>();
	PDI_multi_expose("error", "array_data", array_data_5.data(), PDI_OUT, nullptr);

	{ // check that scalar_data == scalar_data_val
		int scalar_data_read = 0;
		PDI_multi_expose("read_scalar", "scalar_data", &scalar_data_read, PDI_IN, nullptr);
		EXPECT_EQ(scalar_data_val, scalar_data_read);
	}

	{ // check that array_data == array_data_4
		std::array<std::array<int, 4>, 4> array_data_read;
		PDI_multi_expose("read", "array_data", array_data_read.data(), PDI_IN, nullptr);
		EXPECT_EQ(array_data_4, array_data_read);
	}
}

/* test the creation of groups define by differents regex
 */
TEST_F(DeclHdf5, CreateGroupsWithRegexes)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  array_01: { type: array, size: 3, subtype: int }
  array_02: { type: array, size: 3, subtype: int }
  array_03: { type: array, size: 3, subtype: int }
  array_01_read: { type: array, size: 3, subtype: int }
  array_02_read: { type: array, size: 3, subtype: int }
  array_03_read: { type: array, size: 3, subtype: int }
metadata:
  index: int
plugins:
  decl_hdf5:
  - file: decl_hdf5_test_create_different_groups_with_regex.h5
    datasets:
      group/lion_array_data: { type: array, size: 5, subtype: int }
      group[0-9]+/dog_array_data: { type: array, size: 5, subtype: int }
      group_second.*/cat_array_data: { type: array, size: 5, subtype: int }
    write:
      array_01:
        dataset: 'group${index}/dog_array_data'
        dataset_selection: { start: 1, size: 3 }
      array_02:
        dataset: group_second_TRY/cat_array_data
        dataset_selection: { start: 1, size: 3 }
      array_03:
        dataset: group/lion_array_data
        dataset_selection: { start: 1, size: 3 }
    read:
      array_01_read:
        dataset: 'group${index}/dog_array_data'
        dataset_selection: { start: 1, size: 3 }
      array_02_read:
        dataset: group_second_TRY/cat_array_data
        dataset_selection: { start: 1, size: 3 }
      array_03_read:
        dataset: group/lion_array_data
        dataset_selection: { start: 1, size: 3 }
)=="));

	int index = 123;
	PDI_expose("index", &index, PDI_OUT);

	auto const array_01 = make_a<std::array<int, 3>>();
	PDI_expose("array_01", array_01.data(), PDI_OUT);

	auto const array_02 = make_a<std::array<int, 3>>();
	PDI_expose("array_02", array_02.data(), PDI_OUT);

	auto const array_03 = make_a<std::array<int, 3>>();
	PDI_expose("array_03", array_03.data(), PDI_OUT);

	std::array<int, 3> array_01_read;
	PDI_expose("array_01_read", array_01_read.data(), PDI_IN);
	EXPECT_EQ(array_01, array_01_read);

	std::array<int, 3> array_02_read;
	PDI_expose("array_02_read", array_02_read.data(), PDI_IN);
	EXPECT_EQ(array_02, array_02_read);

	std::array<int, 3> array_03_read;
	PDI_expose("array_03_read", array_03_read.data(), PDI_IN);
	EXPECT_EQ(array_03, array_03_read);
}

/* creating different groups defined by the same regex and depended on an index
 */
TEST_F(DeclHdf5, CreateMultipleGroupsFromOneRegex)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  index: int
data:
  array_data: { type: array, size: 3, subtype: int }
  array_data_read: { type: array, size: 3, subtype: int }
plugins:
  decl_hdf5:
    file: decl_hdf5_test_create_groups_that_depend_on_index.h5
    datasets:
      group[0-9]+/array_data: { type: array, size: 5, subtype: int }
    write:
      array_data:
        dataset: 'group${index}/array_data'
        dataset_selection: { start: 1, size: 3 }
    read:
      array_data_read:
        dataset: 'group${index}/array_data'
        dataset_selection: { start: 1, size: 3 }
)=="));

	auto const array_data = make_a<std::array<std::array<int, 3>, 3>>();
	for (int index = 0; index < std::size(array_data); ++index) {
		PDI_expose("index", &index, PDI_OUT);
		PDI_expose("array_data", array_data[index].data(), PDI_OUT);
	}

	for (int index = 0; index < std::size(array_data); ++index) {
		PDI_expose("index", &index, PDI_OUT);
		std::array<int, 3> array_data_read = {};
		PDI_expose("array_data_read", array_data_read.data(), PDI_IN);
		EXPECT_EQ(array_data[index], array_data_read) << "index = " << index;
	}
}

/* check error message generated with a dataset where 4 regex are found
 */

TEST_F(DeclHdf5, ConfigErrorForMultipleRegexes)
{
	InitPdi(PC_parse_string(R"==(
logging:
  level: trace
data:
  array_data: { type: array, subtype: int, size: 3 }
metadata:
  index: int
plugins:
  decl_hdf5:
    file: decl_hdf5_test_four_regex_found.h5
    on_event: write_event
    datasets:
      group[0-9]+/array_data: { type: array, subtype: int, size: 3 }
      group.*/array_data: { type: array, subtype: int, size: 3 }
      group1.*/array_data: { type: array, subtype: int, size: 3 }
      group/.*/array_data: { type: array, subtype: int, size: 3 }
      group12.*/array_data: { type: array, subtype: int, size: 3 }
    write:
      array_data:
        dataset: 'group${index}/array_data'
)=="));

	int const index = 123;
	PDI_expose("index", &index, PDI_OUT);

	auto const array_data = make_a<std::array<int, 3>>();
	EXPECT_CALL(
		*this,
		PdiError(
			Eq(PDI_ERR_SPECTREE),
			StrEq("Error while triggering event `write_event': Spectree_error: "
	              "Found `4' match(es) in the list of datasets section for `group123/array_data'. "
	              "Cannot choose the right element in datasets.\n"
	              "The elements that match `group123/array_data' are:\n"
	              " - group[0-9]+/array_data defined in line 13\n"
	              " - group.*/array_data defined in line 14\n"
	              " - group1.*/array_data defined in line 15\n"
	              " - group12.*/array_data defined in line 17\n"
	              "Attention: The elements are considered as a regex.")
		)
	);
	PDI_multi_expose("write_event", "array_data", array_data.data(), PDI_OUT, nullptr);
}

/* check error when dataset_selection is given and no datasets is found
 */
TEST_F(DeclHdf5, DatasetMissing)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  index: int
data:
  array_data: { type: array, subtype: int, size: 3 }
plugins:
  decl_hdf5:
    file: decl_hdf5_test_no_regex.h5
    write:
      array_data:
        dataset: 'group${index}/array_data'
        dataset_selection: { start: 1, size: 3 }
)=="));

	int const index = 123;
	PDI_expose("index", &index, PDI_OUT);

	auto const array_data = make_a<std::array<int, 3>>();
	EXPECT_CALL(
		*this,
		PdiError(
			Eq(PDI_ERR_SPECTREE),
			StrEq("Unable to share `array_data', Unable to share `array_data', Error while triggering data share `array_data': "
	              "Spectree_error in line 13: Dataset selection is invalid for implicit dataset `group123/array_data'")
		)
	);
	PDI_expose("array_data", array_data.data(), PDI_OUT);
}
