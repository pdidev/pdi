/*******************************************************************************
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2024-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <gtest/gtest.h>
#include <pdi.h>

/*
 * Name:                decl_netcdf_test.01
 *
 * Description:         Tests simple write and read of scalar and array depending on `input' metadata
 */
TEST(decl_netcdf_test, 01)
{
	const char* CONFIG_YAML
		= "logging: trace                                     \n"
		  "metadata:                                          \n"
		  "  input: int                                       \n"
		  "data:                                              \n"
		  "  int_scalar: int                                  \n"
		  "  int_array: {type: array, subtype: int, size: 32} \n"
		  "plugins:                                           \n"
		  "  decl_netcdf:                                     \n"
		  "    - file: 'test_01.nc'                           \n"
		  "      when: '${input}=0'                           \n"
		  "      write: [int_scalar, int_array]               \n"
		  "    - file: 'test_01.nc'                           \n"
		  "      when: '${input}=1'                           \n"
		  "      read: [int_scalar, int_array]                \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	// init data
	int input = 0;
	int int_scalar = 42;
	int int_array[32];
	for (int i = 0; i < 32; i++) {
		int_array[i] = i;
	}

	// write data
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array, PDI_OUT);

	// zero data
	int_scalar = 0;
	for (int i = 0; i < 32; i++) {
		int_array[i] = 0;
	}

	// read data
	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_IN);
	PDI_expose("int_array", int_array, PDI_IN);

	// verify
	printf("%d ?= %d\n", int_scalar, 42);
	ASSERT_EQ(int_scalar, 42);
	for (int i = 0; i < 32; i++) {
		printf("%d ?= %d\n", int_array[i], i);
		ASSERT_EQ(int_array[i], i);
	}

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.02
 *
 * Description:         Tests simple write and read of scalar and array depending on event
 */
TEST(decl_netcdf_test, 02)
{
	const char* CONFIG_YAML
		= "logging: trace                                     \n"
		  "data:                                              \n"
		  "  int_scalar: int                                  \n"
		  "  int_array: {type: array, subtype: int, size: 32} \n"
		  "plugins:                                           \n"
		  "  decl_netcdf:                                     \n"
		  "    - file: 'test_02.nc'                           \n"
		  "      on_event: 'write'                            \n"
		  "      write: [int_scalar, int_array]               \n"
		  "    - file: 'test_02.nc'                           \n"
		  "      on_event: 'read'                             \n"
		  "      read: [int_scalar, int_array]                \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	// init data
	int int_scalar = 42;
	int int_array[32];
	for (int i = 0; i < 32; i++) {
		int_array[i] = i;
	}

	// write data
	PDI_multi_expose("write", "int_scalar", &int_scalar, PDI_OUT, "int_array", int_array, PDI_OUT, NULL);

	// zero data
	int_scalar = 0;
	for (int i = 0; i < 32; i++) {
		int_array[i] = 0;
	}

	// read data
	PDI_multi_expose("read", "int_scalar", &int_scalar, PDI_IN, "int_array", int_array, PDI_IN, NULL);

	// verify
	printf("%d ?= %d\n", int_scalar, 42);
	ASSERT_EQ(int_scalar, 42);
	for (int i = 0; i < 32; i++) {
		printf("%d ?= %d\n", int_array[i], i);
		ASSERT_EQ(int_array[i], i);
	}

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.03
 *
 * Description:         Tests simple write and read of variables and groups attributes
 */
TEST(decl_netcdf_test, 03)
{
	const char* CONFIG_YAML
		= ".vars:                                                   \n"
		  "  - &int_scalar_var                                      \n"
		  "    type: int                                            \n"
		  "    group: 'scalar_group'                                \n"
		  "    attributes:                                          \n"
		  "      scalar_attr: $scalar_attr                          \n"
		  "  - &int_array_var                                       \n"
		  "    type: array                                          \n"
		  "    subtype: int                                         \n"
		  "    size: 32                                             \n"
		  "    group: 'array_group'                                 \n"
		  "    dimensions: ['time']                                 \n"
		  "    attributes:                                          \n"
		  "      array_attr: $array_attr                            \n"
		  ".groups:                                                 \n"
		  "  - &scalar_group_value                                  \n"
		  "    attributes:                                          \n"
		  "      scalar_group_attr: $scalar_group_attr              \n"
		  "  - &array_group_value                                   \n"
		  "    attributes:                                          \n"
		  "      array_group_attr: $array_group_attr                \n"
		  "                                                         \n"
		  "logging: trace                                           \n"
		  "metadata:                                                \n"
		  "  input: int                                             \n"
		  "  group_attr: int                                        \n"
		  "  scalar_attr: int                                       \n"
		  "  array_attr: {type: array, subtype: int, size: 4}       \n"
		  "  scalar_group_attr: int                                 \n"
		  "  array_group_attr: {type: array, subtype: int, size: 4} \n"
		  "data:                                                    \n"
		  "  int_scalar: int                                        \n"
		  "  int_array: {type: array, subtype: int, size: 32}       \n"
		  "plugins:                                                 \n"
		  "  decl_netcdf:                                           \n"
		  "    - file: 'test_03.nc'                                 \n"
		  "      variables:                                         \n"
		  "        int_scalar: *int_scalar_var                      \n"
		  "        int_array: *int_array_var                        \n"
		  "      groups:                                            \n"
		  "        scalar_group: *scalar_group_value                \n"
		  "        array_group: *array_group_value                  \n"
		  "      when: '${input}=0'                                 \n"
		  "      write: [int_scalar, int_array]                     \n"
		  "    - file: 'test_03.nc'                                 \n"
		  "      variables:                                         \n"
		  "        int_scalar: *int_scalar_var                      \n"
		  "        int_array: *int_array_var                        \n"
		  "      groups:                                            \n"
		  "        scalar_group: *scalar_group_value                \n"
		  "        array_group: *array_group_value                  \n"
		  "      when: '${input}=1'                                 \n"
		  "      read: [int_scalar, int_array]                      \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	// init data
	int input = 0;
	int int_scalar = 42;
	int int_array[32];
	for (int i = 0; i < 32; i++) {
		int_array[i] = i;
	}

	// expose attributes
	int scalar_attr = 100;
	PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);

	int array_attr[4];
	array_attr[0] = 101;
	array_attr[1] = 102;
	array_attr[2] = 103;
	array_attr[3] = 104;
	PDI_expose("array_attr", array_attr, PDI_OUT);

	int scalar_group_attr = 200;
	PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);

	int array_group_attr[4];
	array_group_attr[0] = 201;
	array_group_attr[1] = 202;
	array_group_attr[2] = 203;
	array_group_attr[3] = 204;
	PDI_expose("array_group_attr", array_group_attr, PDI_OUT);

	// write data
	input = 0;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array, PDI_OUT);

	// zero data
	int_scalar = 0;
	for (int i = 0; i < 32; i++) {
		int_array[i] = 0;
	}

	// reset metadata
	scalar_attr = 0;
	PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);

	array_attr[0] = 0;
	array_attr[1] = 0;
	array_attr[2] = 0;
	array_attr[3] = 0;
	PDI_expose("array_attr", array_attr, PDI_OUT);

	scalar_group_attr = 0;
	PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);

	array_group_attr[0] = 0;
	array_group_attr[1] = 0;
	array_group_attr[2] = 0;
	array_group_attr[3] = 0;
	PDI_expose("array_group_attr", array_group_attr, PDI_OUT);

	// read data
	input = 1;
	PDI_expose("input", &input, PDI_OUT);

	PDI_share("scalar_attr", &scalar_attr, PDI_INOUT);
	PDI_share("scalar_group_attr", &scalar_group_attr, PDI_INOUT);
	PDI_expose("int_scalar", &int_scalar, PDI_IN);
	PDI_reclaim("scalar_group_attr");
	PDI_reclaim("scalar_attr");

	PDI_share("array_attr", array_attr, PDI_INOUT);
	PDI_share("array_group_attr", array_group_attr, PDI_INOUT);
	PDI_expose("int_array", int_array, PDI_IN);
	PDI_reclaim("array_group_attr");
	PDI_reclaim("array_attr");


	// verify
	printf("scalar_group_attr: %d ?= %d\n", scalar_group_attr, 200);
	ASSERT_EQ(scalar_group_attr, 200);

	printf("scalar_attr: %d ?= %d\n", scalar_attr, 100);
	ASSERT_EQ(scalar_attr, 100);


	printf("array_attr[0]: %d ?= %d\n", array_attr[0], 101);
	ASSERT_EQ(array_attr[0], 101);
	printf("array_attr[1]: %d ?= %d\n", array_attr[1], 102);
	ASSERT_EQ(array_attr[1], 102);
	printf("array_attr[2]: %d ?= %d\n", array_attr[2], 103);
	ASSERT_EQ(array_attr[2], 103);
	printf("array_attr[3]: %d ?= %d\n", array_attr[3], 104);
	ASSERT_EQ(array_attr[3], 104);

	printf("array_group_attr[0]: %d ?= %d\n", array_group_attr[0], 201);
	ASSERT_EQ(array_group_attr[0], 201);
	printf("array_group_attr[1]: %d ?= %d\n", array_group_attr[1], 202);
	ASSERT_EQ(array_group_attr[1], 202);
	printf("array_group_attr[2]: %d ?= %d\n", array_group_attr[2], 203);
	ASSERT_EQ(array_group_attr[2], 203);
	printf("array_group_attr[3]: %d ?= %d\n", array_group_attr[3], 204);
	ASSERT_EQ(array_group_attr[3], 204);

	printf("int_scalar: %d ?= %d\n", int_scalar, 42);
	ASSERT_EQ(int_scalar, 42);
	for (int i = 0; i < 32; i++) {
		printf("%d ?= %d\n", int_array[i], i);
		ASSERT_EQ(int_array[i], i);
	}

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.04
 *
 * Description:         Tests group and variable definitions
 */
TEST(decl_netcdf_test, 04)
{
	const char* CONFIG_YAML
		= ".vars:                                                    \n"
		  "  - &int_scalar_var                                       \n"
		  "    type: int                                             \n"
		  "    attributes:                                           \n"
		  "      scalar_attr: $scalar_attr                           \n"
		  "  - &int_array_var                                        \n"
		  "    type: array                                           \n"
		  "    subtype: int                                          \n"
		  "    size: 32                                              \n"
		  "    dimensions: ['time']                                  \n"
		  "    attributes:                                           \n"
		  "      array_attr: $array_attr                             \n"
		  ".groups:                                                  \n"
		  "  - &scalar_group_value                                   \n"
		  "    attributes:                                           \n"
		  "      scalar_group_attr: $scalar_group_attr               \n"
		  "  - &array_group_value                                    \n"
		  "    attributes:                                           \n"
		  "      array_group_attr: $array_group_attr                 \n"
		  "                                                          \n"
		  "logging: trace                                            \n"
		  "metadata:                                                 \n"
		  "  group_attr: int                                         \n"
		  "  scalar_attr: int                                        \n"
		  "  array_attr: int                                         \n"
		  "  scalar_group_attr: int                                  \n"
		  "  array_group_attr: int                                   \n"
		  "data:                                                     \n"
		  "  int_scalar: int                                         \n"
		  "  int_array: {type: array, subtype: int, size: 32}        \n"
		  "plugins:                                                  \n"
		  "  decl_netcdf:                                            \n"
		  "    - file: 'test_04.nc'                                  \n"
		  "      variables:                                          \n"
		  "        scalar_group/data/int_scalar: *int_scalar_var     \n"
		  "        array_group/data/int_array: *int_array_var        \n"
		  "      groups:                                             \n"
		  "        scalar_group/data: *scalar_group_value            \n"
		  "        array_group/data: *array_group_value              \n"
		  "      on_event: 'write'                                   \n"
		  "      write:                                              \n"
		  "        int_scalar:                                       \n"
		  "          variable: scalar_group/data/int_scalar          \n"
		  "        int_array:                                        \n"
		  "          variable: array_group/data/int_array            \n"
		  "    - file: 'test_04.nc'                                  \n"
		  "      variables:                                          \n"
		  "        scalar_group/data/int_scalar: *int_scalar_var     \n"
		  "        array_group/data/int_array: *int_array_var        \n"
		  "      groups:                                             \n"
		  "        scalar_group/data: *scalar_group_value            \n"
		  "        array_group/data: *array_group_value              \n"
		  "      on_event: 'read'                                    \n"
		  "      read:                                               \n"
		  "        int_scalar:                                       \n"
		  "          variable: scalar_group/data/int_scalar          \n"
		  "        int_array:                                        \n"
		  "          variable: array_group/data/int_array            \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	// init data
	int int_scalar = 42;
	int int_array[32];
	for (int i = 0; i < 32; i++) {
		int_array[i] = i;
	}

	// expose attributes
	int scalar_attr = 100;
	PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);
	int array_attr = 101;
	PDI_expose("array_attr", &array_attr, PDI_OUT);
	int scalar_group_attr = 200;
	PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);
	int array_group_attr = 201;
	PDI_expose("array_group_attr", &array_group_attr, PDI_OUT);

	// write data
	PDI_multi_expose("write", "int_scalar", &int_scalar, PDI_OUT, "int_array", int_array, PDI_OUT, NULL);


	// zero data
	int_scalar = 0;
	for (int i = 0; i < 32; i++) {
		int_array[i] = 0;
	}

	// reset metadata
	scalar_attr = 0;
	PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);
	array_attr = 0;
	PDI_expose("array_attr", &array_attr, PDI_OUT);
	scalar_group_attr = 0;
	PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);
	array_group_attr = 0;
	PDI_expose("array_group_attr", &array_group_attr, PDI_OUT);

	// read data
	PDI_multi_expose(
		"read",
		"int_scalar",
		&int_scalar,
		PDI_IN,
		"int_array",
		int_array,
		PDI_IN,
		"scalar_attr",
		&scalar_attr,
		PDI_INOUT,
		"scalar_group_attr",
		&scalar_group_attr,
		PDI_INOUT,
		"array_attr",
		&array_attr,
		PDI_INOUT,
		"array_group_attr",
		&array_group_attr,
		PDI_INOUT,
		NULL
	);


	// verify
	printf("scalar_group_attr: %d ?= %d\n", scalar_group_attr, 200);
	ASSERT_EQ(scalar_group_attr, 200);

	printf("scalar_attr: %d ?= %d\n", scalar_attr, 100);
	ASSERT_EQ(scalar_attr, 100);

	printf("array_attr: %d ?= %d\n", array_attr, 101);
	ASSERT_EQ(array_attr, 101);

	printf("array_group_attr: %d ?= %d\n", array_group_attr, 201);
	ASSERT_EQ(array_group_attr, 201);

	printf("int_scalar: %d ?= %d\n", int_scalar, 42);
	ASSERT_EQ(int_scalar, 42);
	for (int i = 0; i < 32; i++) {
		printf("%d ?= %d\n", int_array[i], i);
		ASSERT_EQ(int_array[i], i);
	}

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.05
 *
 * Description:         Tests variable selection on write and read
 */
TEST(decl_netcdf_test, 05)
{
	const char* CONFIG_YAML
		= "logging: trace                                  \n"
		  "data:                                           \n"
		  "  int_submatrix_0:                              \n"
		  "    type: array                                 \n"
		  "    subtype: int                                \n"
		  "    size: [4, 4]                                \n"
		  "  int_submatrix_1:                              \n"
		  "    type: array                                 \n"
		  "    subtype: int                                \n"
		  "    size: [4, 4]                                \n"
		  "  int_submatrix_2:                              \n"
		  "    type: array                                 \n"
		  "    subtype: int                                \n"
		  "    size: [4, 4]                                \n"
		  "  int_submatrix_3:                              \n"
		  "    type: array                                 \n"
		  "    subtype: int                                \n"
		  "    size: [4, 4]                                \n"
		  "  int_submatrix_left:                           \n"
		  "    type: array                                 \n"
		  "    subtype: int                                \n"
		  "    size: [8, 4]                                \n"
		  "  int_submatrix_right:                          \n"
		  "    type: array                                 \n"
		  "    subtype: int                                \n"
		  "    size: [8, 4]                                \n"
		  "                                                \n"
		  "plugins:                                        \n"
		  "  decl_netcdf:                                  \n"
		  "    - file: 'test_05.nc'                        \n"
		  "      on_event: 'write'                         \n"
		  "      variables:                                \n"
		  "        int_matrix_var:                         \n"
		  "          type: array                           \n"
		  "          subtype: int                          \n"
		  "          size: [8, 8]                          \n"
		  "          dimensions: ['height', 'width']       \n"
		  "      write:                                    \n"
		  "        int_submatrix_0:                        \n"
		  "          variable: int_matrix_var              \n"
		  "          variable_selection:                   \n"
		  "            start: [0, 0]                       \n"
		  "            subsize: [4, 4]                     \n"
		  "        int_submatrix_1:                        \n"
		  "          variable: int_matrix_var              \n"
		  "          variable_selection:                   \n"
		  "            start: [0, 4]                       \n"
		  "            subsize: [4, 4]                     \n"
		  "        int_submatrix_2:                        \n"
		  "          variable: int_matrix_var              \n"
		  "          variable_selection:                   \n"
		  "            start: [4, 0]                       \n"
		  "            subsize: [4, 4]                     \n"
		  "        int_submatrix_3:                        \n"
		  "          variable: int_matrix_var              \n"
		  "          variable_selection:                   \n"
		  "            start: [4, 4]                       \n"
		  "            subsize: [4, 4]                     \n"
		  "    - file: 'test_05.nc'                        \n"
		  "      on_event: 'read'                          \n"
		  "      variables:                                \n"
		  "        int_matrix_var:                         \n"
		  "          type: array                           \n"
		  "          subtype: int                          \n"
		  "          size: [8, 8]                          \n"
		  "          dimensions: ['height', 'width']       \n"
		  "      read:                                     \n"
		  "        int_submatrix_left:                     \n"
		  "          variable: int_matrix_var              \n"
		  "          variable_selection:                   \n"
		  "            start: [0, 0]                       \n"
		  "            subsize: [8, 4]                     \n"
		  "        int_submatrix_right:                    \n"
		  "          variable: int_matrix_var              \n"
		  "          variable_selection:                   \n"
		  "            start: [0, 4]                       \n"
		  "            subsize: [8, 4]                     \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	// init data
	int int_matrix_0[4][4];
	int int_matrix_1[4][4];
	int int_matrix_2[4][4];
	int int_matrix_3[4][4];
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix_0[i][j] = i * 4 + j;
		}
	}
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix_1[i][j] = 100 + i * 4 + j;
		}
	}
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix_2[i][j] = 200 + i * 4 + j;
		}
	}
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix_3[i][j] = 300 + i * 4 + j;
		}
	}

	// write data
	PDI_multi_expose(
		"write",
		"int_submatrix_0",
		int_matrix_0,
		PDI_OUT,
		"int_submatrix_1",
		int_matrix_1,
		PDI_OUT,
		"int_submatrix_2",
		int_matrix_2,
		PDI_OUT,
		"int_submatrix_3",
		int_matrix_3,
		PDI_OUT,
		NULL
	);

	// zero data
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix_0[i][j] = 0;
		}
	}
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix_1[i][j] = 0;
		}
	}
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix_2[i][j] = 0;
		}
	}
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix_3[i][j] = 0;
		}
	}

	// read data
	int int_matrix_left[8][4];
	int int_matrix_right[8][4];

	PDI_multi_expose("read", "int_submatrix_left", int_matrix_left, PDI_IN, "int_submatrix_right", int_matrix_right, PDI_IN, NULL);

	// verify
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			printf("[%d][%d] %d ?= %d\n", i, j, int_matrix_left[i][j], i * 4 + j);
			ASSERT_EQ(int_matrix_left[i][j], i * 4 + j);
		}
	}
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			printf("[%d][%d] %d ?= %d\n", i, j, int_matrix_right[i][j], 100 + i * 4 + j);
			ASSERT_EQ(int_matrix_right[i][j], 100 + i * 4 + j);
		}
	}

	for (int i = 4; i < 8; i++) {
		for (int j = 0; j < 4; j++) {
			printf("[%d][%d] %d ?= %d\n", i, j, int_matrix_left[i][j], 200 + (i - 4) * 4 + j);
			ASSERT_EQ(int_matrix_left[i][j], 200 + (i - 4) * 4 + j);
		}
	}

	for (int i = 4; i < 8; i++) {
		for (int j = 0; j < 4; j++) {
			printf("[%d][%d] %d ?= %d\n", i, j, int_matrix_right[i][j], 300 + (i - 4) * 4 + j);
			ASSERT_EQ(int_matrix_right[i][j], 300 + (i - 4) * 4 + j);
		}
	}

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.06
 *
 * Description:         Tests infinite dimension
 */
TEST(decl_netcdf_test, 06)
{
	const char* CONFIG_YAML
		= "logging: trace                                      \n"
		  "data:                                               \n"
		  "  iter: int                                         \n"
		  "  int_matrix:                                       \n"
		  "    type: array                                     \n"
		  "    subtype: int                                    \n"
		  "    size: [8, 8]                                    \n"
		  "                                                    \n"
		  "plugins:                                            \n"
		  "  decl_netcdf:                                      \n"
		  "    - file: 'test_06.nc'                            \n"
		  "      on_event: 'write'                             \n"
		  "      variables:                                    \n"
		  "        int_matrix_var:                             \n"
		  "          type: array                               \n"
		  "          subtype: int                              \n"
		  "          size: [0, 8, 8]                           \n"
		  "          dimensions: ['iter', 'height', 'width']   \n"
		  "      write:                                        \n"
		  "        int_matrix:                                 \n"
		  "          variable: int_matrix_var                  \n"
		  "          variable_selection:                       \n"
		  "            start: ['$iter', 0, 0]                  \n"
		  "            subsize: [1, 8, 8]                      \n"
		  "    - file: 'test_06.nc'                            \n"
		  "      on_event: 'read'                              \n"
		  "      variables:                                    \n"
		  "        int_matrix_var:                             \n"
		  "          type: array                               \n"
		  "          subtype: int                              \n"
		  "          size: [0, 8, 8]                           \n"
		  "      read:                                         \n"
		  "        int_matrix:                                 \n"
		  "          variable: int_matrix_var                  \n"
		  "          variable_selection:                       \n"
		  "            start: ['$iter', 0, 0]                  \n"
		  "            subsize: [1, 8, 8]                      \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	int int_matrix[8][8];
	for (int iter = 0; iter < 32; iter++) {
		// init data
		for (int i = 0; i < 8; i++) {
			for (int j = 0; j < 8; j++) {
				int_matrix[i][j] = iter * 100 + i * 8 + j;
			}
		}

		// write data
		PDI_multi_expose("write", "iter", &iter, PDI_OUT, "int_matrix", int_matrix, PDI_OUT, NULL);
	}

	for (int iter = 0; iter < 32; iter++) {
		// read data
		PDI_multi_expose("read", "iter", &iter, PDI_OUT, "int_matrix", int_matrix, PDI_IN, NULL);

		// verify
		for (int i = 0; i < 8; i++) {
			for (int j = 0; j < 8; j++) {
				printf("[%d][%d] %d ?= %d\n", i, j, int_matrix[i][j], iter * 100 + i * 8 + j);
				ASSERT_EQ(int_matrix[i][j], iter * 100 + i * 8 + j);
			}
		}
	}

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.07
 *
 * Description:         Tests yaml syntaxe with `write: data`
 */
TEST(decl_netcdf_test, 07)
{
	const char* CONFIG_YAML
		= "logging: trace                                      \n"
		  "data:                                               \n"
		  "  int_matrix:                                       \n"
		  "    type: array                                     \n"
		  "    subtype: int                                    \n"
		  "    size: [8, 8]                                    \n"
		  "                                                    \n"
		  "plugins:                                            \n"
		  "  decl_netcdf:                                      \n"
		  "    - file: 'test_07.nc'                            \n"
		  "      on_event: 'write'                             \n"
		  "      write: int_matrix                             \n";

	// Use the null error handle explicitly. Otherwise, the error is not captured.
	// Need further investigation
	PDI_errhandler(PDI_NULL_HANDLER);

	ASSERT_EQ(PDI_OK, PDI_init(PC_parse_string(CONFIG_YAML)));

	int int_matrix[8][8];

	// init data
	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			int_matrix[i][j] = 100 + i * 8 + j;
		}
	}

	// write data
	ASSERT_EQ(PDI_OK, PDI_multi_expose("write", "int_matrix", int_matrix, PDI_OUT, NULL));

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.size_of
 *
 * Description:         Tests simple write and read of scalar and array depending on `input' metadata
 */
TEST(decl_netcdf_test, size_of)
{
	const char* CONFIG_YAML
		= "logging: trace                                      \n"
		  "metadata:                                           \n"
		  "  input: int                                        \n"
		  "data:                                               \n"
		  "  int_scalar: int                                   \n"
		  "  int_array: {type: array, subtype: int, size: 32}  \n"
		  "  array_size: int                                   \n"
		  "                                                    \n"
		  "plugins:                                            \n"
		  "  decl_netcdf:                                      \n"
		  "    - file: 'test_07s.nc'                            \n"
		  "      when: '${input}=0'                            \n"
		  "      write: [int_scalar, int_array]                \n"
		  "    - file: 'test_07s.nc'                            \n"
		  "      when: '${input}=1'                            \n"
		  "      read:                                         \n"
		  "        int_scalar:                                 \n"
		  "        int_array:                                  \n"
		  "        array_size:                                 \n"
		  "          size_of: int_array                        \n";

	PDI_init(PC_parse_string(CONFIG_YAML));
	// init data
	int input = 0;
	int int_scalar = 42;
	int int_array[32];
	for (int i = 0; i < 32; i++) {
		int_array[i] = i;
	}

	// write data
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array, PDI_OUT);

	// zero data
	int_scalar = 0;
	for (int i = 0; i < 32; i++) {
		int_array[i] = 0;
	}

	// read data
	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	int array_size = 0;
	PDI_expose("array_size", &array_size, PDI_IN);
	PDI_expose("int_scalar", &int_scalar, PDI_IN);
	PDI_expose("int_array", int_array, PDI_IN);

	// verify
	printf("array_size = %d\n", array_size);
	printf("%d ?= %d\n", int_scalar, 42);
	ASSERT_EQ(int_scalar, 42);
	ASSERT_EQ(array_size, 32);
	for (int i = 0; i < 32; i++) {
		// printf("%d ?= %d\n", int_array[i], i);
		ASSERT_EQ(int_array[i], i);
	}

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.defalte
 *
 * Description:         Tests simple write and read of compressed variables
*/
TEST(decl_netcdf_test, deflate)
{
	constexpr char CONFIG_YAML[] = R"(
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
    )";

	PDI_init(PC_parse_string(CONFIG_YAML));

	// init data
	int input = 0;
	int int_scalar = 42;
	int N = 1000;
	int chunk = 1000;
	int* int_array = new int[N];
	int* int_matrix = new int[N * N];

	// write data
	for (int i = 0; i < N; i++) {
		int_array[i] = i;
	}

	for (int i = 0; i < N * N; i++) {
		int_matrix[i] = i;
	}

	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("pb_size", &N, PDI_OUT);
	PDI_expose("chunk", &chunk, PDI_OUT);

	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array, PDI_OUT);
	PDI_expose("int_matrix", int_matrix, PDI_OUT);

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
	int_scalar = 0;
	for (int i = 0; i < N; i++) {
		int_array[i] = 0;
	}
	for (int i = 0; i < N * N; i++) {
		int_matrix[i] = 0;
	}

	PDI_expose("input", &input, PDI_OUT);

	PDI_expose("int_scalar", &int_scalar, PDI_IN);
	PDI_expose("int_array", int_array, PDI_IN);
	PDI_expose("int_matrix", int_matrix, PDI_IN);

	// verify
	ASSERT_EQ(int_scalar, 42);

	for (int i = 0; i < N; i++) {
		ASSERT_EQ(int_array[i], i);
	}

	for (int i = 0; i < N * N; i++) {
		ASSERT_EQ(int_matrix[i], i);
	}

	delete[] int_array;
	delete[] int_matrix;

	PDI_finalize();
}

/*
 * Name:                decl_netcdf_test.scalar_read
 *
 * Description:         Tests write and read of scalar of type mismatch
 */
TEST(decl_netcdf_test, scalar_read)
{
	const char* CONFIG_YAML
		= "logging: trace                                      \n"
		//   "metadata:                                           \n"
		  "data:                                               \n"
		  "  int_scalar: int32                                   \n"
		  "  int_scalar_read: int64                            \n"
		  "                                                    \n"
		  "plugins:                                            \n"
		  "  decl_netcdf:                                      \n"
		  "    - file: 'test_read.nc'                           \n"
		  "      variable:                                      \n"
          "        int_scalar_32: int32                         \n"  
		  "      on_event: write_data                            \n"
		  "      write:                                         \n"
		  "        int_scalar:                                  \n"
		  "          variable: int_scalar_32                    \n"
		  "    - file: 'test_read.nc'                           \n"
		  "      on_event: read_data                           \n"
		  "      variable:                                      \n"
          "        int_scalar_32: int32                         \n"
		  "      read:                                         \n"
		  "        int_scalar_read:                            \n"
		  "          variable:                                  \n"
		  "            int_scalar_32                            \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	int32_t int_scalar = 42;
	
	// write data
	PDI_multi_expose("write_data", "int_scalar", &int_scalar, PDI_OUT, NULL);

	// read data
	int64_t int_scalar_read=-1;
	int status = PDI_multi_expose("read_data", "int_scalar_read", &int_scalar_read, PDI_IN, NULL);
	ASSERT_NE(int_scalar_read, 42);    
	
	PDI_finalize();
}
