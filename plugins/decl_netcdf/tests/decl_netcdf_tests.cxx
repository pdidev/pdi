/*******************************************************************************
 * Copyright (C) 2024-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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

#include <array>
#include <filesystem>

#include <pdi/testing.h>

class DeclNetcdfTest: public ::PDI::PdiTest
{};

/*
 * Name:                DeclNetcdfTest.01
 *
 * Description:         Tests simple write and read of scalar and array depending on `input' metadata
 */
TEST_F(DeclNetcdfTest, 01)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  input: int
data:
  int_scalar: int
  int_array: {type: array, subtype: int, size: 32}
plugins:
  decl_netcdf:
    - file: 'test_01.nc'
      when: '${input}=0'
      write: [int_scalar, int_array]
    - file: 'test_01.nc'
      when: '${input}=1'
      read: [int_scalar, int_array]
)=="));

	// init data
	int input = 0;
	int const int_scalar = 42;
	auto const int_array = make_a<std::array<int, 32>>();

	// write data
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array.data(), PDI_OUT);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_01.nc"));

	// read_data
	int int_scalar_read = 0;
	std::array<int, 32> int_array_read{};
	int_array_read.fill(-1);

	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar_read, PDI_IN);
	PDI_expose("int_array", int_array_read.data(), PDI_IN);

	// verify
	ASSERT_EQ(int_scalar, int_scalar_read);
	ASSERT_EQ(int_array, int_array_read);
}

/*
 * Name:                DeclNetcdfTest.02
 *
 * Description:         Tests simple write and read of scalar and array depending on event
 */
TEST_F(DeclNetcdfTest, 02)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  input: int
data:
  int_scalar: int
  int_array: {type: array, subtype: int, size: 32}
plugins:
  decl_netcdf:
    - file: 'test_02.nc'
      on_event: 'write'
      write: [int_scalar, int_array]
    - file: 'test_02.nc'
      on_event: 'read'
      read: [int_scalar, int_array]
)=="));

	// init data
	int const int_scalar = 42;
	auto const int_array = make_a<std::array<int, 32>>();

	// write data
	PDI_multi_expose("write", "int_scalar", &int_scalar, PDI_OUT, "int_array", int_array.data(), PDI_OUT, NULL);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_02.nc"));

	// read data
	int int_scalar_read = 0;
	std::array<int, 32> int_array_read{};
	int_array_read.fill(0);

	PDI_multi_expose("read", "int_scalar", &int_scalar_read, PDI_IN, "int_array", int_array_read.data(), PDI_IN, NULL);

	// verify
	ASSERT_EQ(int_scalar, int_scalar_read);
	ASSERT_EQ(int_array, int_array_read);
}

/*
 * Name:                DeclNetcdfTest.03
 *
 * Description:         Tests simple write and read of variables and groups attributes
 */
TEST_F(DeclNetcdfTest, 03)
{
	InitPdi(PC_parse_string(R"==(
.vars:
  - &int_scalar_var
    type: int
    group: 'scalar_group'
    attributes:
      scalar_attr: $scalar_attr
  - &int_array_var
    type: array
    subtype: int
    size: 32
    group: 'array_group'
    dimensions: ['time']
    attributes:
      array_attr: $array_attr
.groups:
  - &scalar_group_value
    attributes:
      scalar_group_attr: $scalar_group_attr
  - &array_group_value
    attributes:
      array_group_attr: $array_group_attr

logging: trace
metadata:
  input: int
  group_attr: int
  scalar_attr: int
  array_attr: {type: array, subtype: int, size: 4}
  scalar_group_attr: int
  array_group_attr: {type: array, subtype: int, size: 4}
data:
  int_scalar: int
  int_array: {type: array, subtype: int, size: 32}
plugins:
  decl_netcdf:
    - file: 'test_03.nc'
      variables:
        int_scalar: *int_scalar_var
        int_array: *int_array_var
      groups:
        scalar_group: *scalar_group_value
        array_group: *array_group_value
      when: '${input}=0'
      write: [int_scalar, int_array]
    - file: 'test_03.nc'
      variables:
        int_scalar: *int_scalar_var
        int_array: *int_array_var
      groups:
        scalar_group: *scalar_group_value
        array_group: *array_group_value
      when: '${input}=1'
      read: [int_scalar, int_array]
)=="));

	// init data
	int input = 0;
	int const int_scalar = 42;
	auto const int_array = make_a<std::array<int, 32>>();

	// init and expose attributes
	int scalar_attr = 100;
	PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);

	auto const array_attr = make_a<std::array<int, 4>>();
	PDI_expose("array_attr", array_attr.data(), PDI_OUT);

	int const scalar_group_attr = 200;
	PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);

	auto const array_group_attr = make_a<std::array<int, 4>>();
	PDI_expose("array_group_attr", array_group_attr.data(), PDI_OUT);

	// write data
	input = 0;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array.data(), PDI_OUT);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_03.nc"));

	// reset metadata attributes
	int scalar_attr_reset_read = 0;
	PDI_expose("scalar_attr", &scalar_attr_reset_read, PDI_OUT);
	int scalar_group_attr_reset_read = 0;
	PDI_expose("scalar_group_attr", &scalar_group_attr_reset_read, PDI_OUT);
	std::array<int, 4> array_attr_reset_read{};
	array_attr_reset_read.fill(0);
	PDI_expose("array_attr", array_attr_reset_read.data(), PDI_OUT);
	std::array<int, 4> array_group_attr_reset_read{};
	array_group_attr_reset_read.fill(0);
	PDI_expose("array_group_attr", array_group_attr_reset_read.data(), PDI_OUT);

	// read data and attributes
	int int_scalar_read = 0;
	std::array<int, 32> int_array_read{};
	int_array_read.fill(0);

	input = 1;
	PDI_expose("input", &input, PDI_OUT);

	PDI_share("scalar_attr", &scalar_attr_reset_read, PDI_INOUT);
	PDI_share("scalar_group_attr", &scalar_group_attr_reset_read, PDI_INOUT);
	PDI_expose("int_scalar", &int_scalar_read, PDI_IN);
	PDI_reclaim("scalar_group_attr");
	PDI_reclaim("scalar_attr");

	PDI_share("array_attr", array_attr_reset_read.data(), PDI_INOUT);
	PDI_share("array_group_attr", array_group_attr_reset_read.data(), PDI_INOUT);
	PDI_expose("int_array", int_array_read.data(), PDI_IN);
	PDI_reclaim("array_group_attr");
	PDI_reclaim("array_attr");

	// verify attributes
	ASSERT_EQ(scalar_attr, scalar_attr_reset_read);
	ASSERT_EQ(scalar_group_attr, scalar_group_attr_reset_read);

	ASSERT_EQ(array_attr, array_attr_reset_read);
	ASSERT_EQ(array_group_attr, array_group_attr_reset_read);

	// verify data
	ASSERT_EQ(int_scalar, int_scalar_read);
	ASSERT_EQ(int_array, int_array_read);
}

/*
 * Name:                DeclNetcdfTest.04
 *
 * Description:         Tests group and variable definitions
 */
TEST_F(DeclNetcdfTest, 04)
{
	InitPdi(PC_parse_string(R"==(
.vars:
  - &int_scalar_var
    type: int
    attributes:
      scalar_attr: $scalar_attr
  - &int_array_var
    type: array
    subtype: int
    size: 32
    dimensions: ['time']
    attributes:
      array_attr: $array_attr
.groups:
  - &scalar_group_value
    attributes:
      scalar_group_attr: $scalar_group_attr
  - &array_group_value
    attributes:
      array_group_attr: $array_group_attr

logging: trace
metadata:
  group_attr: int
  scalar_attr: int
  array_attr: int
  scalar_group_attr: int
  array_group_attr: int
data:
  int_scalar: int
  int_array: {type: array, subtype: int, size: 32}
plugins:
  decl_netcdf:
    - file: 'test_04.nc'
      variables:
        scalar_group/data/int_scalar: *int_scalar_var
        array_group/data/int_array: *int_array_var
      groups:
        scalar_group/data: *scalar_group_value
        array_group/data: *array_group_value
      on_event: 'write'
      write:
        int_scalar:
          variable: scalar_group/data/int_scalar
        int_array:
          variable: array_group/data/int_array
    - file: 'test_04.nc'
      variables:
        scalar_group/data/int_scalar: *int_scalar_var
        array_group/data/int_array: *int_array_var
      groups:
        scalar_group/data: *scalar_group_value
        array_group/data: *array_group_value
      on_event: 'read'
      read:
        int_scalar:
          variable: scalar_group/data/int_scalar
        int_array:
          variable: array_group/data/int_array
)=="));

	// init data
	int const int_scalar = 42;
	auto const int_array = make_a<std::array<int, 32>>();

	// init and expose attributes
	int const scalar_attr = 100;
	PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);
	int const array_attr = 101;
	PDI_expose("array_attr", &array_attr, PDI_OUT);
	int const scalar_group_attr = 200;
	PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);
	int const array_group_attr = 201;
	PDI_expose("array_group_attr", &array_group_attr, PDI_OUT);

	// write data
	PDI_multi_expose("write", "int_scalar", &int_scalar, PDI_OUT, "int_array", int_array.data(), PDI_OUT, NULL);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_04.nc"));

	// reset metadata attributes
	int scalar_attr_reset_read = 0;
	PDI_expose("scalar_attr", &scalar_attr_reset_read, PDI_OUT);
	int array_attr_reset_read = 0;
	PDI_expose("array_attr", &array_attr_reset_read, PDI_OUT);
	int scalar_group_attr_reset_read = 0;
	PDI_expose("scalar_group_attr", &scalar_group_attr_reset_read, PDI_OUT);
	int array_group_attr_reset_read = 0;
	PDI_expose("array_group_attr", &array_group_attr_reset_read, PDI_OUT);

	// read data
	int int_scalar_read = 0;
	std::array<int, 32> int_array_read{};

	PDI_multi_expose(
		"read",
		"int_scalar",
		&int_scalar_read,
		PDI_IN,
		"int_array",
		int_array_read.data(),
		PDI_IN,
		"scalar_attr",
		&scalar_attr_reset_read,
		PDI_INOUT,
		"scalar_group_attr",
		&scalar_group_attr_reset_read,
		PDI_INOUT,
		"array_attr",
		&array_attr_reset_read,
		PDI_INOUT,
		"array_group_attr",
		&array_group_attr_reset_read,
		PDI_INOUT,
		NULL
	);

	// verify attributes
	ASSERT_EQ(scalar_attr, scalar_attr_reset_read);
	ASSERT_EQ(scalar_group_attr, scalar_group_attr_reset_read);

	ASSERT_EQ(array_attr, array_attr_reset_read);
	ASSERT_EQ(array_group_attr, array_group_attr_reset_read);

	// verify data
	ASSERT_EQ(int_scalar, int_scalar_read);
	ASSERT_EQ(int_array, int_array_read);
}

/*
 * Name:                DeclNetcdfTest.05
 *
 * Description:         Tests variable selection on write and read
 */
TEST_F(DeclNetcdfTest, 05)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  int_submatrix_0:
    type: array
    subtype: int
    size: [4, 4]
  int_submatrix_1:
    type: array
    subtype: int
    size: [4, 4]
  int_submatrix_2:
    type: array
    subtype: int
    size: [4, 4]
  int_submatrix_3:
    type: array
    subtype: int
    size: [4, 4]
  int_submatrix_left:
    type: array
    subtype: int
    size: [8, 4]
  int_submatrix_right:
    type: array
    subtype: int
    size: [8, 4]
plugins:
  decl_netcdf:
    - file: 'test_05.nc'
      on_event: 'write'
      variables:
        int_matrix_var:
          type: array
          subtype: int
          size: [8, 8]
          dimensions: ['height', 'width']
      write:
        int_submatrix_0:
          variable: int_matrix_var
          variable_selection:
            start: [0, 0]
            subsize: [4, 4]
        int_submatrix_1:
          variable: int_matrix_var
          variable_selection:
            start: [0, 4]
            subsize: [4, 4]
        int_submatrix_2:
          variable: int_matrix_var
          variable_selection:
            start: [4, 0]
            subsize: [4, 4]
        int_submatrix_3:
          variable: int_matrix_var
          variable_selection:
            start: [4, 4]
            subsize: [4, 4]
    - file: 'test_05.nc'
      on_event: 'read'
      variables:
        int_matrix_var:
          type: array
          subtype: int
          size: [8, 8]
          dimensions: ['height', 'width']
      read:
        int_submatrix_left:
          variable: int_matrix_var
          variable_selection:
            start: [0, 0]
            subsize: [8, 4]
        int_submatrix_right:
          variable: int_matrix_var
          variable_selection:
            start: [0, 4]
            subsize: [8, 4]
)=="));

	// init data
	auto const int_matrix_0 = make_a<std::array<std::array<int, 4>, 4>>();
	auto const int_matrix_1 = make_a<std::array<std::array<int, 4>, 4>>();
	auto const int_matrix_2 = make_a<std::array<std::array<int, 4>, 4>>();
	auto const int_matrix_3 = make_a<std::array<std::array<int, 4>, 4>>();

	EXPECT_NE(int_matrix_0, int_matrix_1);
	EXPECT_NE(int_matrix_0, int_matrix_2);
	EXPECT_NE(int_matrix_0, int_matrix_3);
	EXPECT_NE(int_matrix_1, int_matrix_2);
	EXPECT_NE(int_matrix_1, int_matrix_3);
	EXPECT_NE(int_matrix_2, int_matrix_3);

	// write data
	PDI_multi_expose(
		"write",
		"int_submatrix_0",
		int_matrix_0.data(),
		PDI_OUT,
		"int_submatrix_1",
		int_matrix_1.data(),
		PDI_OUT,
		"int_submatrix_2",
		int_matrix_2.data(),
		PDI_OUT,
		"int_submatrix_3",
		int_matrix_3.data(),
		PDI_OUT,
		NULL
	);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_05.nc"));

	// read data
	std::array<std::array<int, 4>, 8> int_matrix_left{};
	std::array<std::array<int, 4>, 8> int_matrix_right{};

	PDI_multi_expose("read", "int_submatrix_left", int_matrix_left.data(), PDI_IN, "int_submatrix_right", int_matrix_right.data(), PDI_IN, NULL);

	/*
		                     |   int_matrix_0    |
		int_matrix_left  =   |-------------------|
		                     |   int_matrix_2    |

		                     |   int_matrix_1    |
		int_matrix_right  =  |-------------------|
		                     |   int_matrix_3    |
	*/

	// verify
	for (int i = 0; i < 4; i++) {
		ASSERT_EQ(int_matrix_left[i], int_matrix_0[i]) << "Error in row " << i << " of int_matrix_left";
	}

	for (int i = 4; i < 8; i++) {
		ASSERT_EQ(int_matrix_left[i], int_matrix_2[i-4]) << "Error in row " << i << " of int_matrix_left";
	}

	for (int i = 0; i < 4; i++) {
		ASSERT_EQ(int_matrix_right[i], int_matrix_1[i]) << "Error in row " << i << " of int_matrix_left";
	}

	for (int i = 4; i < 8; i++) {
		ASSERT_EQ(int_matrix_right[i], int_matrix_3[i-4]) << "Error in row " << i << " of int_matrix_right";
	}
}

/*
 * Name:                DeclNetcdfTest.06
 *
 * Description:         Tests infinite dimension
 */
TEST_F(DeclNetcdfTest, 06)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  iter: int
  int_matrix:
    type: array
    subtype: int
    size: [8, 8]
plugins:
  decl_netcdf:
    - file: 'test_06.nc'
      on_event: 'write'
      variables:
        int_matrix_var:
          type: array
          subtype: int
          size: [0, 8, 8]
          dimensions: ['iter', 'height', 'width']
      write:
        int_matrix:
          variable: int_matrix_var
          variable_selection:
            start: ['$iter', 0, 0]
            subsize: [1, 8, 8]
    - file: 'test_06.nc'
      on_event: 'read'
      variables:
        int_matrix_var:
          type: array
          subtype: int
          size: [0, 8, 8]
      read:
        int_matrix:
          variable: int_matrix_var
          variable_selection:
            start: ['$iter', 0, 0]
            subsize: [1, 8, 8]
)=="));

	// init data
	auto const int_matrix = make_a<std::array<std::array<std::array<int, 8>, 8>,32>>();

	for (int iter = 0; iter < 32; iter++) {
		// write data
		PDI_multi_expose("write", "iter", &iter, PDI_OUT, "int_matrix", int_matrix[iter].data(), PDI_OUT, NULL);
	}

	std::array<std::array<int, 8>, 8> int_matrix_read{};
	for (int iter = 0; iter < 32; iter++) {

		// read data
		for (auto & row : int_matrix_read) {
			row.fill(0); // reinitialize to zero int_matrix_read
		}
		PDI_multi_expose("read", "iter", &iter, PDI_OUT, "int_matrix", int_matrix_read.data(), PDI_IN, NULL);

		// verify
		ASSERT_EQ(int_matrix[iter],int_matrix_read);
	}
}

/*
 * Name:                DeclNetcdfTest.07
 *
 * Description:         Tests yaml syntaxe with `write: data`
 */
TEST_F(DeclNetcdfTest, 07)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  int_matrix:
    type: array
    subtype: int
    size: [8, 8]
plugins:
  decl_netcdf:
    - file: 'test_07.nc'
      on_event: 'write'
      write: int_matrix
)=="));

	// init data
	auto const int_matrix = make_a<std::array<std::array<int, 8>, 8>>();

	// write data
	ASSERT_EQ(PDI_OK, PDI_multi_expose("write", "int_matrix", int_matrix.data(), PDI_OUT, NULL));

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_07.nc"));
}

/*
 * Name:                DeclNetcdfTest.size_of
 *
 * Description:         Tests simple write and read of scalar and array depending on `input' metadata
 */
TEST_F(DeclNetcdfTest, size_of)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  input: int
data:
  int_scalar: int
  int_array: {type: array, subtype: int, size: 32}
  array_size: int
plugins:
  decl_netcdf:
    - file: 'test_07s.nc'
      when: '${input}=0'
      write: [int_scalar, int_array]
    - file: 'test_07s.nc'
      when: '${input}=1'
      read:
        int_scalar:
        int_array:
        array_size:
          size_of: int_array
)=="));

	// init data
	int input = 0;
	int const array_size = 32;
	int const int_scalar = 42;
	auto const int_array = make_a<std::array<int, array_size>>();

	// write data
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array.data(), PDI_OUT);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_07s.nc"));

	// read data
	int array_size_read = 0;
	int int_scalar_read = 0;
	std::array<int, array_size> int_array_read{};

	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("array_size", &array_size_read, PDI_IN);
	PDI_expose("int_scalar", &int_scalar_read, PDI_IN);
	PDI_expose("int_array", int_array_read.data(), PDI_IN);

	// verify
	ASSERT_EQ(array_size, array_size_read);
	ASSERT_EQ(int_scalar, int_scalar_read);
	ASSERT_EQ(int_array, int_array_read);
}

/*
 * Name:                DeclNetcdfTest.defalte
 *
 * Description:         Tests simple write and read of compressed variables
*/
TEST_F(DeclNetcdfTest, deflate)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  pb_size: int
  input: int
  chunk: int
data:
  int_scalar: int
  int_array: {type: array, subtype: int, size: $pb_size}
  int_matrix:
    type: array
    subtype: int
    size: ['$pb_size', '$pb_size']
plugins:
  decl_netcdf:
    - file: 'test_deflate_0.nc'
      variables:
        int_scalar: int
        int_array:
          type: array
          subtype: int
          size: $pb_size
          dimensions: ['time']
        int_matrix:
          type: array
          subtype: int
          size: ['$pb_size', '$pb_size']
          dimensions: ['col', 'row']
      when: '${input}=0'
      write: [int_scalar, int_array, int_matrix]
    - file: 'test_deflate_6.nc'
      variables:
        int_scalar: int
        int_array:
          type: array
          subtype: int
          size: $pb_size
          dimensions: ['time']
          deflate: 6
        int_matrix:
          type: array
          subtype: int
          size: ['$pb_size', '$pb_size']
          dimensions: ['col', 'row']
          deflate: 6
      when: '${input}=0'
      write: [int_scalar, int_array, int_matrix]
    - file: 'test_deflate_9.nc'
      deflate: 9
      variables:
        int_scalar: int
        int_array:
          type: array
          subtype: int
          size: $pb_size
          dimensions: ['time']
          chunking: $chunk
        int_matrix:
          type: array
          subtype: int
          size: ['$pb_size', '$pb_size']
          dimensions: ['col', 'row']
          chunking: ['$chunk', '$chunk']
      when: '${input}=0'
      write: [int_scalar, int_array, int_matrix]
    - file: 'test_deflate_mix.nc'
      deflate: 6
      variables:
        int_scalar: int
        int_array:
          type: array
          subtype: int
          size: $pb_size
          dimensions: ['time']
          deflate: 9
          chunking: $chunk
        int_matrix:
          type: array
          subtype: int
          size: ['$pb_size', '$pb_size']
          dimensions: ['col', 'row']
          chunking: ['$chunk', '$chunk']
      when: '${input}=0'
      write: [int_scalar, int_array, int_matrix]
    - file: 'test_deflate_6.nc'
      when: '${input}=1'
      read: [int_scalar, int_array, int_matrix]
)=="));

	// init data
	int input = 0;
	int const int_scalar = 42;
	int const N = 1000;
	int const chunk = 1000;

	auto const int_array = make_a<std::array<int, N>>();
	auto const int_matrix = make_a<std::array<std::array<int, N>, N>>();

	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("pb_size", &N, PDI_OUT);
	PDI_expose("chunk", &chunk, PDI_OUT);

	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array.data(), PDI_OUT);
	PDI_expose("int_matrix", int_matrix.data(), PDI_OUT);

	// check the deflate level of output files
	int result;
	result = std::system("which ncdump > /dev/null 2>&1");
	// check the deflate level only if ncdump is available
	if (result == 0) {
		result = std::system("ncdump -hs test_deflate_6.nc | grep -q '_DeflateLevel = 6'");
		EXPECT_EQ(result, 0) << "Deflate level for test_deflate_6.nc is not 6";
		result = std::system("ncdump -hs test_deflate_9.nc | grep -q '_DeflateLevel = 9'");
		EXPECT_EQ(result, 0) << "Deflate level for test_deflate_9.nc is not 9";
		result = std::system("ncdump -hs test_deflate_mix.nc | grep -q 'int_array:_DeflateLevel = 9'");
		EXPECT_EQ(result, 0) << "Deflate level of int_array in test_deflate_mix.nc is not 9";
		result = std::system("ncdump -hs test_deflate_mix.nc | grep -q 'int_matrix:_DeflateLevel = 6'");
		EXPECT_EQ(result, 0) << "Deflate level of int_matrix in test_deflate_mix.nc is not 6";
	}

	// read data
	input = 1;
	int int_scalar_read;
	std::array<int, N> int_array_read{};
	std::array<std::array<int, N>, N> int_matrix_read{};

	PDI_expose("input", &input, PDI_OUT);

	PDI_expose("int_scalar", &int_scalar_read, PDI_IN);
	PDI_expose("int_array", int_array_read.data(), PDI_IN);
	PDI_expose("int_matrix", int_matrix_read.data(), PDI_IN);

	// verify
	ASSERT_EQ(int_scalar, int_scalar_read);

	ASSERT_EQ(int_array, int_array_read);

	for (int ii = 0; ii < N; ii++) {
		ASSERT_EQ(int_matrix[ii], int_matrix_read[ii]);
	}
}


/*
 * Name:                DeclNetcdfTest.IntReadMismatch
 *
 * Description:         Tests write and read of int with type mismatch
 */
TEST_F(DeclNetcdfTest, IntReadMismatch)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  int_in: int32
  int_out: int64
plugins:
  decl_netcdf:
    - file: 'test_int_read.nc'
      on_event: write_data
      write:
        int_in:
          variable: scalar_int32
    - file: 'test_int_read.nc'
      on_event: read_data
      read:
        int_out:
          variable:
            scalar_int32
)=="));

	// init data
	int32_t int_in = 42;

	// write data
	PDI_multi_expose("write_data", "int_in", &int_in, PDI_OUT, NULL);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_int_read.nc"));

	EXPECT_CALL(
		*this,
		PdiError(
			testing::Eq(PDI_ERR_TYPE),
			testing::AllOf(
				testing::HasSubstr("while triggering `read_data',"),
				testing::HasSubstr("Decl_netcdf plugin: Datatype mismatch (with size): "
	                               "read 'scalar_int32' of size 4 for a buffer of size 8")
			)
		)
	);

	// read data
	int64_t int_out = -1;
	EXPECT_EQ(PDI_ERR_TYPE, PDI_multi_expose("read_data", "int_out", &int_out, PDI_IN, NULL));
}

/*
 * Name:                DeclNetcdfTest.FloatReadMismatch
 *
 * Description:         Tests write and read of float/double with type mismatch
 */
TEST_F(DeclNetcdfTest, FloatReadMismatch)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  var_in: float
  var_out: double
plugins:
  decl_netcdf:
    - file: 'test_float_read.nc'
      on_event: write_data
      write:
        var_in:
          variable: scalar_float
    - file: 'test_float_read.nc'
      on_event: read_data
      read:
        var_out:
          variable: scalar_float
)=="));

	// init data
	float var_in = 12.34;

	// write data
	PDI_multi_expose("write_data", "var_in", &var_in, PDI_OUT, NULL);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_float_read.nc"));

	EXPECT_CALL(
		*this,
		PdiError(
			testing::Eq(PDI_ERR_TYPE),
			testing::AllOf(
				testing::HasSubstr("while triggering `read_data',"),
				testing::HasSubstr("Decl_netcdf plugin: Datatype mismatch (with size): "
	                               "read 'scalar_float' of size 4 for a buffer of size 8")
			)
		)
	);

	// read data
	double var_out = -1.0;
	EXPECT_EQ(PDI_ERR_TYPE, PDI_multi_expose("read_data", "var_out", &var_out, PDI_IN, NULL));
}

/*
 * Name:                DeclNetcdfTest.ReadDataNotDefinedInYaml
 *
 * Description:         Tests write and read of float/double which is not defined in Yaml
 *                      Read with on_event
 */
TEST_F(DeclNetcdfTest, ReadDataNotDefinedInYamlCaseOnEvent)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  var_in: float
plugins:
  decl_netcdf:
    - file: 'test_float_read_data_not_defined.nc'
      on_event: write_data
      write:
        var_in:
          variable: scalar_float
    - file: 'test_float_read_data_not_defined.nc'
      on_event: read_data
      read:
        var_out:
          variable: scalar_float
)=="));

	// init data
	float var_in = 15.34;

	// write data
	PDI_multi_expose("write_data", "var_in", &var_in, PDI_OUT, NULL);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_float_read_data_not_defined.nc"));

	EXPECT_CALL(
		*this,
		PdiError(
			testing::Eq(PDI_ERR_TYPE),
			testing::AllOf(
				testing::HasSubstr("while triggering `read_data',"),
				testing::HasSubstr("can not read `scalar_float'"),
				testing::HasSubstr("the type of the exposed data `var_out' is undefined (likely not "
	                               "listed in (meta)data section of the specification tree).")
			)
		)
	);

	// read data
	float var_out = -1.0;
	EXPECT_EQ(PDI_ERR_TYPE, PDI_multi_expose("read_data", "var_out", &var_out, PDI_IN, NULL));
}

/*
 * Name:                DeclNetcdfTest.ReadDataNotDefinedInYamlCaseOnData
 *
 * Description:         Tests write and read of float/double which is not defined in Yaml
 *                      Read with on_data
 */
TEST_F(DeclNetcdfTest, ReadDataNotDefinedInYamlCaseOnData)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  var_in: float
plugins:
  decl_netcdf:
    - file: 'test_float_read_data_not_defined.nc'
      on_event: write_data
      write:
        var_in:
          variable: scalar_float
    - file: 'test_float_read_data_not_defined.nc'
      on_data: var_out
      read:
        var_out:
          variable: scalar_float
)=="));

	// init data
	float var_in = 15.34;

	// write data
	PDI_multi_expose("write_data", "var_in", &var_in, PDI_OUT, NULL);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_float_read_data_not_defined.nc"));

	EXPECT_CALL(
		*this,
		PdiError(
			testing::Eq(PDI_ERR_TYPE),
			testing::AllOf(
				testing::HasSubstr("while sharing `var_out'"),
				testing::HasSubstr("can not read `scalar_float'"),
				testing::HasSubstr("the type of the exposed data `var_out' is undefined (likely not "
	                               "listed in (meta)data section of the specification tree).")
			)
		)
	);

	// read data
	float var_out = -1.0;
	PDI_expose("var_out", &var_out, PDI_IN);
}

/*
 * Name:                DeclNetcdfTest.ReadDoubleArrayNotDefinedInYaml
 *
 * Description:         Tests write and read of double array that not defined in Yaml
 */
TEST_F(DeclNetcdfTest, ReadDoubleArrayNotDefinedInYaml)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  nn: int
data:
  array_in: {type: array, subtype: double, size: ['$nn']}
plugins:
  decl_netcdf:
    - file: 'test_double_array_read.nc'
      variables:
        nc_var: {type: array, subtype: double, size: ['$nn']}
      on_event: write_data
      write:
        array_in:
          variable: nc_var
    - file: 'test_double_array_read.nc'
      variables:
        nc_var:  {type: array, subtype: double, size: ['$nn']}
      on_event: read_data
      read:
        array_out:
          variable: nc_var
)=="));

	// init data
	int const nn = 3;
	auto const array_in = make_a<std::array<double, nn>>();

	PDI_expose("nn", &nn, PDI_INOUT);

	// write data
	PDI_multi_expose("write_data", "array_in", array_in.data(), PDI_OUT, NULL);

	// check file exists
	EXPECT_TRUE(std::filesystem::exists("test_double_array_read.nc"));

	EXPECT_CALL(
		*this,
		PdiError(
			testing::Eq(PDI_ERR_TYPE),
			testing::AllOf(
				testing::HasSubstr("while triggering `read_data',"),
				testing::HasSubstr("can not read `nc_var'"),
				testing::HasSubstr("the type of the exposed data `array_out' is undefined (likely not "
	                               "listed in (meta)data section of the specification tree).")
			)
		)
	);

	// read data
	std::array<double, nn> array_out{};
	array_out.fill(0.0);
	PDI_multi_expose("read_data", "array_out", array_out.data(), PDI_IN, NULL);
}
