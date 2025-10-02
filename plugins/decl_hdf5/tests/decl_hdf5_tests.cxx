/*******************************************************************************
 * Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <gtest/gtest.h>
#include <unistd.h>
#include <pdi.h>

class decl_hdf5_test: public ::testing::Test
{
protected:
	decl_hdf5_test() {}

	virtual ~decl_hdf5_test() {}

	void SetUp(const std::string& filename)
	{
		m_file_to_delete = filename;
		std::remove(m_file_to_delete.c_str()); // In case of the tear down is not called in a previous test
	}

	void TearDown() override
	{
		// If the API of PDI throw, this function is not called
		// If the macro FAIL() of gtest is called, this function is not called
		std::cout << "decl_hdf5_test:TearDown: remove file " << m_file_to_delete << std::endl;
		std::remove(m_file_to_delete.c_str());
	}

private:
	std::string m_file_to_delete;
};

// ===========================================
// test for checking error

/*
* Structure to check the error message of PDI
*/

struct context_check_error {
	std::string true_errmsg;
	PDI_status_t true_err_status;
	int has_failed;
};

/*
* function to redefine the error handler of PDI
*/

void succeed_on_failure(PDI_status_t status, const char* message, void* ctx)
{
	if (status) {
		context_check_error* tmp_ctx = static_cast<struct context_check_error*>(ctx);
		EXPECT_EQ(ctx, tmp_ctx);
		EXPECT_TRUE(status == (PDI_status_t)tmp_ctx->true_err_status)
			<< "error: status = " << status << " should be: " << (PDI_status_t)tmp_ctx->true_err_status << "\n";
		std::string true_errmsg = (std::string)tmp_ctx->true_errmsg;
		EXPECT_STREQ(message, true_errmsg.c_str());
		tmp_ctx->has_failed = 1; // has_failed = 1
	}
}

void succeed_on_failure_without_checking_message(PDI_status_t status, const char* message, void* ctx)
{
	if (status) {
		context_check_error* tmp_ctx = static_cast<struct context_check_error*>(ctx);
		EXPECT_EQ(ctx, tmp_ctx);
		EXPECT_TRUE(status == (PDI_status_t)tmp_ctx->true_err_status)
			<< "error: status = " << status << " should be: " << (PDI_status_t)tmp_ctx->true_err_status << "\n";
		tmp_ctx->has_failed = 1; // has_failed = 1
	}
}

/*
 * Name:                decl_hdf5_test.01
 *
 * Description:         Metatadata export using filename
 */
TEST_F(decl_hdf5_test, 01)
{
	SetUp("5.h5");

	const char* CONFIG_YAML
		= "logging: trace                \n"
		  "metadata:                     \n"
		  "  meta0: int                  \n"
		  "  meta1: int                  \n"
		  "  meta2: int                  \n"
		  "  meta3: int                  \n"
		  "  meta4: int                  \n"
		  "data:                         \n"
		  "  test_var: double            \n"
		  "plugins:                      \n"
		  "  decl_hdf5:                  \n"
		  "    file: ${meta1}.h5         \n"
		  "    write:                    \n"
		  "        test_var: ~           \n"
		  "        meta2: ~              \n";

	int value[5] = {5, 4, 3, 2, 1};
	double test_var = 0;

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	PDI_multi_expose(
		"testing",
		"meta0",
		&value[0],
		PDI_OUT,
		"meta1",
		&value[0],
		PDI_OUT,
		"meta2",
		&value[1],
		PDI_OUT,
		"meta3",
		&value[2],
		PDI_OUT,
		"meta4",
		&value[3],
		PDI_OUT,
		"test_var",
		&test_var,
		PDI_OUT,
		NULL
	);

	PDI_finalize();
	PC_tree_destroy(&conf);

	FILE* fp = fopen("5.h5", "r");
	EXPECT_NE(fp, nullptr) << "File not found. \n";
	fclose(fp);
}

/*
 * Name:                decl_hdf5_test.02
 *
 * Description:         outer record
 */
TEST_F(decl_hdf5_test, 02)
{
	SetUp("decl_hdf5_test_outer_record.h5");

	const char* CONFIG_YAML
		= "logging: trace                             \n"
		  "metadata:                                  \n"
		  "  input: int                               \n"
		  "data:                                      \n"
		  "  outer_record:                            \n"
		  "    type: struct                           \n"
		  "    members:                               \n"
		  "      - id: int                            \n"
		  "      - value:                             \n"
		  "          type: array                      \n"
		  "          size: [2, 2]                     \n"
		  "          subtype:                         \n"
		  "            type: struct                   \n"
		  "            members:                       \n"
		  "              - x: int                     \n"
		  "              - y: int                     \n"
		  "              - z: int                     \n"
		  "plugins:                                   \n"
		  "  decl_hdf5:                               \n"
		  "    - file: decl_hdf5_test_outer_record.h5 \n"
		  "      write: [outer_record]                \n"
		  "      when: $input=0                       \n"
		  "    - file: decl_hdf5_test_outer_record.h5 \n"
		  "      read: [outer_record]                 \n"
		  "      when: $input=1                       \n";

	struct XYZ {
		int x;
		int y;
		int z;
	};

	struct Record {
		int id;
		struct XYZ value[4]; // 2 x 2
	};

	struct Record outer_record;

	// init data
	outer_record.id = 24;
	for (int i = 0; i < 4; i++) {
		outer_record.value[i].x = -1 * i;
		outer_record.value[i].y = 2 * i;
		outer_record.value[i].z = 3 * i;
	}

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	int input = 0;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("outer_record", &outer_record, PDI_OUT);

	// reset record
	outer_record.id = 0;
	for (int i = 0; i < 4; i++) {
		outer_record.value[i].x = 0;
		outer_record.value[i].y = 0;
		outer_record.value[i].z = 0;
	}

	// load record from file
	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("outer_record", &outer_record, PDI_IN);

	// check values
	printf("%d != %d\n", outer_record.id, 24);
	ASSERT_EQ(outer_record.id, 24);
	for (int i = 0; i < 4; i++) {
		EXPECT_EQ(outer_record.value[i].x, -1 * i);
		EXPECT_EQ(outer_record.value[i].y, 2 * i);
		EXPECT_EQ(outer_record.value[i].z, 3 * i);
	}
	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.03
 *
 * Description:         record with dataset_selection and memory_selection
 */
TEST_F(decl_hdf5_test, 03)
{
	SetUp("decl_hdf5_test_record_and_selection.h5");
	const char* CONFIG_YAML
		= "logging: trace                                      \n"
		  "metadata:                                           \n"
		  "  input: int                                        \n"
		  "data:                                               \n"
		  "  array_of_record:                                  \n"
		  "    type: array                                     \n"
		  "    size: 4                                         \n"
		  "    subtype:                                        \n"
		  "      type: struct                                  \n"
		  "      members:                                      \n"
		  "        - id: int                                   \n"
		  "        - value:                                    \n"
		  "            type: array                             \n"
		  "            subtype: int                            \n"
		  "            size: [4, 4]                            \n"
		  "  array_of_record_read:                             \n"
		  "    type: array                                     \n"
		  "    size: 2                                         \n"
		  "    subtype:                                        \n"
		  "      type: struct                                  \n"
		  "      members:                                      \n"
		  "        - id: int                                   \n"
		  "        - value:                                    \n"
		  "            type: array                             \n"
		  "            subtype: int                            \n"
		  "            size: [4, 4]                            \n"
		  "plugins:                                            \n"
		  "  decl_hdf5:                                        \n"
		  "    - file: decl_hdf5_test_record_and_selection.h5  \n"
		  "      when: $input=0                                \n"
		  "      datasets:                                     \n"
		  "        data_array:                                 \n"
		  "          type: array                               \n"
		  "          size: 2                                   \n"
		  "          subtype:                                  \n"
		  "            type: struct                            \n"
		  "            members:                                \n"
		  "              - id: int                             \n"
		  "              - value:                              \n"
		  "                  type: array                       \n"
		  "                  subtype: int                      \n"
		  "                  size: [4, 4]                      \n"
		  "      write:                                        \n"
		  "        array_of_record:                            \n"
		  "          dataset: data_array                       \n"
		  "          memory_selection:                         \n"
		  "            size: 2                                 \n"
		  "            start: 1                                \n"
		  "          dataset_selection:                        \n"
		  "            size: 2                                 \n"
		  "    - file: decl_hdf5_test_record_and_selection.h5  \n"
		  "      when: $input=1                                \n"
		  "      read: [array_of_record_read]                  \n";

	struct Record {
		int id;
		int value[16];
	};

	struct Record rec[4];
	// init data
	for (int i = 0; i < 4; i++) {
		rec[i].id = i;
		for (int j = 0; j < 16; j++) {
			rec[i].value[j] = j + i * 16;
		}
	}
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	int input = 0;
	PDI_expose("input", &input, PDI_OUT);

	// write record to file
	PDI_expose("array_of_record", rec, PDI_OUT);

	// load record from file
	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	struct Record rec_read[2];
	PDI_expose("array_of_record", rec_read, PDI_IN);

	// check values
	for (int i = 1; i < 3; i++) {
		EXPECT_EQ(rec[i].id, i);
		for (int j = 0; j < 16; j++) {
			EXPECT_EQ(rec[i].value[j], j + i * 16);
		}
	}
	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.04
 *
 * Description:         test attribute depend on group
 */
TEST_F(decl_hdf5_test, 04)
{
	SetUp("decl_hdf5_test_attribute_group.h5");
	//PDI_write
	const char* CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  array_size: int                                                       \n"
		  "data:                                                                   \n"
		  "  array_data: { size: $array_size, type: array, subtype: int }          \n"
		  "  group_attr: float                                                     \n"
		  "  dset_attr: {type: array, subtype: int, size: 4}                       \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    file: decl_hdf5_test_attribute_group.h5                             \n"
		  "    datasets:                                                           \n"
		  "      data/array_data: { size: $array_size, type: array, subtype: int } \n"
		  "    write:                                                              \n"
		  "      array_data:                                                       \n"
		  "        dataset: data/array_data                                        \n"
		  "        attributes:                                                     \n"
		  "          expr_attr: $array_size                                        \n"
		  "      dset_attr:                                                        \n"
		  "        attribute: data/array_data#dset_attr_name                       \n"
		  "      group_attr:                                                       \n"
		  "        attribute: data#group_attr_name                                 \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int array_size = 10;
	int test_array[array_size];
	for (int i = 0; i < 10; i++) {
		test_array[i] = i;
	}
	PDI_expose("array_size", &array_size, PDI_OUT);
	PDI_expose("array_data", test_array, PDI_OUT);

	float group_attr = 1.2345f;
	PDI_expose("group_attr", &group_attr, PDI_OUT);

	int dset_attr[4];
	for (int i = 0; i < 4; i++) {
		dset_attr[i] = i;
	}
	PDI_expose("dset_attr", dset_attr, PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	//PDI_read

	CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  array_size: int                                                       \n"
		  "data:                                                                   \n"
		  "  array_data: { size: 10, type: array, subtype: int }                   \n"
		  "  dset_attr: {type: array, subtype: int, size: 4}                       \n"
		  "  group_attr: float                                                     \n"
		  "  expr_attr: int                                                        \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    file: decl_hdf5_test_attribute_group.h5                             \n"
		  "    datasets:                                                           \n"
		  "      data/array_data: { size: 10, type: array, subtype: int }          \n"
		  "    read:                                                               \n"
		  "      array_data:                                                       \n"
		  "        dataset: data/array_data                                        \n"
		  "        attributes:                                                     \n"
		  "          expr_attr: $expr_attr                                         \n"
		  "      dset_attr:                                                        \n"
		  "        attribute: data/array_data#dset_attr_name                       \n"
		  "      group_attr:                                                       \n"
		  "        attribute: data#group_attr_name                                 \n";

	conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	array_size = 10;
	for (int i = 0; i < 10; i++) {
		test_array[i] = 0;
	}
	PDI_expose("array_size", &array_size, PDI_OUT);

	group_attr = 0.0f;
	for (int i = 0; i < 4; i++) {
		dset_attr[i] = 0;
	}

	PDI_expose("group_attr", &group_attr, PDI_IN);
	EXPECT_EQ(group_attr, 1.2345f) << "group_attr invalid value: " << group_attr << " (should be: 1.2345)";

	PDI_expose("dset_attr", dset_attr, PDI_IN);
	for (int i = 0; i < 4; i++) {
		EXPECT_EQ(dset_attr[i], i) << "dset_attr[" << i << "] invalid value: " << dset_attr[i] << " (should be: " << i << ")";
	}

	int expr_attr = 0;
	PDI_share("expr_attr", &expr_attr, PDI_IN);
	PDI_expose("array_data", test_array, PDI_IN);
	for (int i = 0; i < 10; i++) {
		EXPECT_EQ(test_array[i], i) << "test_array[" << i << "] invalid value: " << test_array[i] << "(should be: " << i << ")";
	}
	EXPECT_EQ(expr_attr, 10) << "expr_attr invalid value: " << expr_attr << " (should be: 10)";
	PDI_reclaim("expr_attr");

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.05
 *
 * Description:         test attribute depend on data
 */

TEST_F(decl_hdf5_test, 05)
{
	SetUp("decl_hdf5_test_attribute_data.h5");

	//PDI_write
	const char* CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  array_size: int                                                       \n"
		  "data:                                                                   \n"
		  "  array_data: { size: $array_size, type: array, subtype: int }          \n"
		  "  dset_attr: {type: array, subtype: int, size: 4}                       \n"
		  "  size_attr: int                                                        \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    file: decl_hdf5_test_attribute_data.h55                             \n"
		  "    on_event: \"write\"                                                 \n"
		  "    write: [array_data, array_data#dset_attr, array_data#size_attr]     \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int array_size = 10;
	int test_array[array_size];
	for (int i = 0; i < 10; i++) {
		test_array[i] = i;
	}
	PDI_expose("array_size", &array_size, PDI_OUT);
	PDI_expose("array_data", test_array, PDI_OUT);

	int dset_attr[4];
	for (int i = 0; i < 4; i++) {
		dset_attr[i] = i;
	}
	PDI_multi_expose("write", "size_attr", &array_size, PDI_OUT, "array_data", test_array, PDI_OUT, "dset_attr", dset_attr, PDI_OUT, NULL);

	PDI_finalize();
	PC_tree_destroy(&conf);

	//PDI_read
	CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  array_size: int                                                       \n"
		  "data:                                                                   \n"
		  "  array_data: { size: $array_size, type: array, subtype: int }          \n"
		  "  dset_attr: {type: array, subtype: int, size: 4}                       \n"
		  "  size_attr: int                                                        \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    file: decl_hdf5_test_attribute_data.h55                             \n"
		  "    on_event: \"read\"                                                  \n"
		  "    read: [array_data, array_data#dset_attr, array_data#size_attr]      \n";

	conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	array_size = 10;
	for (int i = 0; i < 10; i++) {
		test_array[i] = 0;
	}
	PDI_expose("array_size", &array_size, PDI_OUT);

	float group_attr = 0.0f;
	for (int i = 0; i < 4; i++) {
		dset_attr[i] = 0;
	}

	int size_attr = 0;
	PDI_multi_expose("read", "size_attr", &size_attr, PDI_IN, "array_data", test_array, PDI_IN, "dset_attr", dset_attr, PDI_IN, NULL);

	EXPECT_EQ(size_attr, 10) << "size_attr invalid value: " << size_attr << " (should be: 10)";

	for (int i = 0; i < 4; i++) {
		EXPECT_EQ(dset_attr[i], i) << "dset_attr[" << i << "] invalid value: " << dset_attr[i] << " (should be: " << i << ")";
	}

	for (int i = 0; i < 10; i++) {
		EXPECT_EQ(test_array[i], i) << "test_array[" << i << "] invalid value: " << test_array[i] << " (should be: " << i << ")";
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.06
 *
 * Description:         read dataset size before dataset itself
 */
TEST_F(decl_hdf5_test, 06)
{
	SetUp("decl_hdf5_test_read_dataset_size_before_dataset_itself.h5");
	//PDI_write
	const char* CONFIG_YAML
		= "logging: trace                                                          \n"
		  "data:                                                                   \n"
		  "  array_data: { size: 10, type: array, subtype: int }                   \n"
		  "  matrix_data: { size: [10, 10], type: array, subtype: float }          \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    file: decl_hdf5_test_read_dataset_size_before_dataset_itself.h5     \n"
		  "    write: [array_data, matrix_data]                                    \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int array_data[10];
	for (int i = 0; i < 10; i++) {
		array_data[i] = i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);

	int matrix_data[10][10];
	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 10; j++) {
			matrix_data[i][j] = 10 * i + j;
		}
	}
	PDI_expose("matrix_data", matrix_data, PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	//PDI_read
	CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  input: int                                                            \n"
		  "data:                                                                   \n"
		  "  array_data_size: int64                                                \n"
		  "  matrix_data_size: { size: 2, type: array, subtype: int64 }            \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    - file: decl_hdf5_test_read_dataset_size_before_dataset_itself.h5   \n"
		  "      when: $input                                                      \n"
		  "      read:                                                             \n"
		  "        array_data_size:                                                \n"
		  "          size_of: array_data                                           \n"
		  "        matrix_data_size:                                               \n"
		  "          size_of: matrix_data                                          \n"
		  "    - file: decl_hdf5_test_read_dataset_size_before_dataset_itself.h5   \n"
		  "      on_event: \"read_size\"                                           \n"
		  "      read:                                                             \n"
		  "        array_data_size:                                                \n"
		  "          size_of: array_data                                           \n"
		  "        matrix_data_size:                                               \n"
		  "          size_of: matrix_data                                          \n";

	conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int input = 1;
	PDI_expose("input", &input, PDI_OUT);

	long array_size = 0;
	long matrix_size[2] = {0, 0};
	PDI_expose("array_data_size", &array_size, PDI_IN);
	PDI_expose("matrix_data_size", matrix_size, PDI_IN);

	EXPECT_EQ(array_size, 10) << "array_size invalid value: " << array_size << " (should be: 10)";
	EXPECT_TRUE(matrix_size[0] == 10 || matrix_size[1] == 10)
		<< "matrix_size invalid value: [" << matrix_size[0] << ", " << matrix_size[1] << "] (should be: [10, 10]";

	// now with event
	input = 0;
	PDI_expose("input", &input, PDI_OUT);

	array_size = 0;
	matrix_size[0] = 0;
	matrix_size[1] = 0;
	PDI_multi_expose("read_size", "array_data_size", &array_size, PDI_IN, "matrix_data_size", matrix_size, PDI_IN, NULL);

	EXPECT_EQ(array_size, 10) << "array_size invalid value: " << array_size << " (should be: 10)";
	EXPECT_TRUE(matrix_size[0] == 10 || matrix_size[1] == 10)
		<< "matrix_size invalid value: [" << matrix_size[0] << ", " << matrix_size[1] << "] (should be: [10, 10]";

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.07
 *
 * Description:         different dimension of data and dataset
 */
TEST_F(decl_hdf5_test, 07)
{
	SetUp("decl_hdf5_test_different_dim_of_data_and_dataset.h5");

	//PDI_write
	const char* CONFIG_YAML
		= "logging: trace                                                 \n"
		  "data:                                                          \n"
		  "  scalar_data: double                                          \n"
		  "  array_data: { size: [8, 8], type: array, subtype: int }      \n"
		  "plugins:                                                       \n"
		  "  decl_hdf5:                                                   \n"
		  "    file: decl_hdf5_test_different_dim_of_data_and_dataset.h5  \n"
		  "    datasets:                                                  \n"
		  "      scalar_data: {type: array, subtype: double, size: 1}     \n"
		  "      array_data: {type: array, subtype: int, size: [4, 4, 4]} \n"
		  "    write: [scalar_data, array_data]                           \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	double scalar_data = 1.2345;
	PDI_expose("scalar_data", &scalar_data, PDI_OUT);

	int array_data[8][8];
	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			array_data[i][j] = i * 8 + j;
		}
	}

	PDI_expose("array_data", array_data, PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	//PDI_read
	CONFIG_YAML
		= "logging: trace                                                 \n"
		  "data:                                                          \n"
		  "  scalar_data: double                                          \n"
		  "  array_data: { size: [8, 8], type: array, subtype: int }      \n"
		  "plugins:                                                       \n"
		  "  decl_hdf5:                                                   \n"
		  "    file: decl_hdf5_test_different_dim_of_data_and_dataset.h5  \n"
		  "    datasets:                                                  \n"
		  "      scalar_data: {type: array, subtype: double, size: 1}     \n"
		  "      array_data: {type: array, subtype: int, size: [4, 4, 4]} \n"
		  "    read: [scalar_data, array_data]                            \n";

	conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	scalar_data = 0;
	PDI_expose("scalar_data", &scalar_data, PDI_IN);
	EXPECT_EQ(scalar_data, 1.2345) << "Wrong value of scalar_data: " << scalar_data << " != 1.2345";

	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			array_data[i][j] = 0;
		}
	}

	PDI_expose("array_data", array_data, PDI_IN);

	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			EXPECT_EQ(array_data[i][j], i * 8 + j) << "Wrong value of array_data[" << i << "][" << j << "]: " << array_data[i][j]
												   << " != " << i * 8 + j;
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.08
 *
 * Description:         
 */
//  TEST(decl_hdf5_test, 08) {

// 	//PDI write
// 		const char* CONFIG_YAML =
// 			"logging: trace                                                 \n"
// 			"data:                                                          \n"
// 			"  array_data: { size: 10, type: array, subtype: int }          \n"
// 			"plugins:                                                       \n"
// 			"  decl_hdf5:                                                   \n"
// 			"    file: decl_hdf5_test_10.h5                                 \n"
// 			// "    datasets:                                                  \n"
// 			// "      array_data: {type: array, subtype: int, size: 10}        \n"
// 			"    write:                                                     \n"
// 			"      array_data:                                              \n"
// 			"        - memory_selection:                                    \n"
// 			"            size: 10                                           \n"
// 			"          dataset_selection:                                   \n"
// 			"            size: 10                                           \n"
// 			;

// 		PC_tree_t conf = PC_parse_string(CONFIG_YAML);
// 		PDI_init(conf);

// 		int test_array[10];
// 		for (int i = 0; i < 10; i++) {
// 			test_array[i] = i;
// 		}

// 		PDI_expose("array_data", test_array, PDI_OUT);

// 		PDI_finalize();
// 		PC_tree_destroy(&conf);

// 	//PDI read
// 		CONFIG_YAML =
// 			"logging: trace                                                 \n"
// 			"data:                                                          \n"
// 			"  array_data: { size: 10, type: array, subtype: int }          \n"
// 			"plugins:                                                       \n"
// 			"  decl_hdf5:                                                   \n"
// 			"    file: decl_hdf5_test_10.h5                                 \n"
// 			"    read: [array_data]                                         \n"
// 			;

// 		PC_tree_t conf = PC_parse_string(CONFIG_YAML);
// 		PDI_init(conf);

// 		test_array[10];
// 		for (int i = 0; i < 10; i++) {
// 			test_array[i] = 0;
// 		}
// 		PDI_expose("array_data", test_array, PDI_IN);

// 		for (int i = 0; i < 10; i++) {
// 			if (test_array[i] != i) {
// 				fprintf(stderr, "[%d] %d != %d\n ", i, test_array[i], i);
// 				return 1;
// 			}
// 		}

// 		PDI_finalize();
// 		PC_tree_destroy(&conf);

// 	int status = PDI_write();
// 	ASSERT_EQ (status, 0) << "Status Error - PDI_write" << status;

// 	status = PDI_read();
// 	ASSERT_EQ (status, 0) << "Status Error - PDI_write" << status;
// }

/*
 * Name:                decl_hdf5_test.09
 *
 * Description:         collision policy
 */
TEST_F(decl_hdf5_test, 08)
{
	SetUp("decl_hdf5_test_collision_policy.h5");

	const char* CONFIG_YAML
		= "logging: trace                                                 \n"
		  "data:                                                          \n"
		  "  scalar_data: int                                             \n"
		  "  array_data: { size: [4, 4], type: array, subtype: int }      \n"
		  "plugins:                                                       \n"
		  "  decl_hdf5:                                                   \n"
		  "    - file: decl_hdf5_test_collision_policy.h5                 \n"
		  "      on_event: init                                           \n"
		  "      datasets:                                                \n"
		  "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
		  "      write: [array_data]                                      \n"
		  "    - file: decl_hdf5_test_collision_policy.h5                 \n"
		  "      collision_policy: skip                                   \n"
		  "      on_event: skip                                           \n"
		  "      datasets:                                                \n"
		  "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
		  "      write: [array_data]                                      \n"
		  "    - file: decl_hdf5_test_collision_policy.h5                 \n"
		  "      collision_policy: write_into                             \n"
		  "      on_event: write_into                                     \n"
		  "      datasets:                                                \n"
		  "        scalar_data: int                                       \n"
		  "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
		  "      write: [scalar_data, array_data]                         \n"
		  "    - file: decl_hdf5_test_collision_policy.h5                 \n"
		  "      collision_policy: replace                                \n"
		  "      on_event: replace                                        \n"
		  "      datasets:                                                \n"
		  "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
		  "      write: [array_data]                                      \n"
		  "    - file: decl_hdf5_test_collision_policy.h5                 \n"
		  "      collision_policy: write_into                             \n"
		  "      on_event: append                                         \n"
		  "      datasets:                                                \n"
		  "        scalar_data: int                                       \n"
		  "      write:                                                   \n"
		  "        scalar_data:                                           \n"
		  "          collision_policy: error                              \n"
		  "    - file: decl_hdf5_test_collision_policy.h5                 \n"
		  "      collision_policy: error                                  \n"
		  "      on_event: error                                          \n"
		  "      datasets:                                                \n"
		  "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
		  "      write: [array_data]                                      \n"
		  "    - file: decl_hdf5_test_collision_policy.h5                 \n"
		  "      on_event: read                                           \n"
		  "      datasets:                                                \n"
		  "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
		  "      read: [array_data]                                       \n"
		  "    - file: decl_hdf5_test_collision_policy.h5                 \n"
		  "      on_event: read_scalar                                    \n"
		  "      datasets:                                                \n"
		  "        scalar_data: int                                       \n"
		  "      read: [scalar_data]                                      \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	// INIT
	int array_data[4][4];
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 0;
		}
	}

	PDI_multi_expose("init", "array_data", array_data, PDI_OUT, NULL);

	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 1;
		}
	}

	// SKIP
	PDI_multi_expose("skip", "array_data", array_data, PDI_OUT, NULL);

	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = -1;
		}
	}

	PDI_multi_expose("read", "array_data", array_data, PDI_IN, NULL);

	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			if (array_data[i][j] != 0) {
				printf("array_data[%d][%d] = %d, should be: 0", i, j, array_data[i][j]);
				PDI_finalize();
				PC_tree_destroy(&conf);
				FAIL();
			}
		}
	}

	// WRITE_INTO
	int scalar_data = 42;
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 2;
		}
	}
	PDI_multi_expose("write_into", "scalar_data", &scalar_data, PDI_OUT, "array_data", array_data, PDI_OUT, NULL);
	scalar_data = -1;
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = -1;
		}
	}
	PDI_multi_expose("read_scalar", "scalar_data", &scalar_data, PDI_IN, NULL);
	if (scalar_data != 42) {
		printf("scalar_data = %d, should be: 42", scalar_data);
		PDI_finalize();
		PC_tree_destroy(&conf);
		FAIL();
	}
	PDI_multi_expose("read", "array_data", array_data, PDI_IN, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			if (array_data[i][j] != 2) {
				printf("array_data[%d][%d] = %d, should be: 2", i, j, array_data[i][j]);
				PDI_finalize();
				PC_tree_destroy(&conf);
				FAIL();
			}
		}
	}

	// REPLACE
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 3;
		}
	}
	PDI_multi_expose("replace", "array_data", array_data, PDI_OUT, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = -1;
		}
	}
	PDI_multi_expose("read", "array_data", array_data, PDI_IN, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			if (array_data[i][j] != 3) {
				printf("array_data[%d][%d] = %d, should be: 3", i, j, array_data[i][j]);
				PDI_finalize();
				PC_tree_destroy(&conf);
				FAIL();
			}
		}
	}

	// check error status
	int has_failed = 0;
	std::string true_errmsg
		= "Error while triggering event `read_scalar': System_error: Cannot open `scalar_data' dataset object 'scalar_data' doesn't exist\n";
	PDI_status_t true_status = PDI_ERR_SYSTEM;
	context_check_error ctx{true_errmsg, true_status, has_failed};

	// defined local error handler
	PDI_errhandler_t local_errhandler;
	local_errhandler.func = succeed_on_failure_without_checking_message;
	local_errhandler.context = static_cast<void*>(&ctx);

	PDI_errhandler_t std_handler = PDI_errhandler(local_errhandler); //changing err handler
	PDI_status_t status = PDI_multi_expose("read_scalar", "scalar_data", &scalar_data, PDI_IN, NULL);

	EXPECT_EQ(ctx.has_failed, 1) << "Error expected but not reported (has_failed=" << ctx.has_failed << "), terminating \n";
	if (ctx.has_failed != 1) {
		PDI_finalize();
		PC_tree_destroy(&conf);
		FAIL();
	}

	PDI_errhandler(std_handler); // changing to standart handler


	// APPEND
	scalar_data = 42;
	PDI_multi_expose("append", "scalar_data", &scalar_data, PDI_OUT, NULL);
	scalar_data = -1;
	PDI_multi_expose("read_scalar", "scalar_data", &scalar_data, PDI_IN, NULL);
	if (scalar_data != 42) {
		printf("scalar_data = %d, should be: 42", scalar_data);
		PDI_finalize();
		PC_tree_destroy(&conf);
		FAIL();
	}

	// change value of the context of the handler
	ctx.has_failed = 0;
	ctx.true_err_status = PDI_ERR_SYSTEM;
	ctx.true_errmsg = "Error while triggering event `append': System_error: Dataset collision `scalar_data': Dataset already exists";

	std_handler = PDI_errhandler(local_errhandler); //changing err handler
	status = PDI_multi_expose("append", "scalar_data", &scalar_data, PDI_OUT, NULL);
	EXPECT_EQ(ctx.has_failed, 1) << "Error expected but not reported (has_failed=" << ctx.has_failed << "), terminating \n";
	if (ctx.has_failed != 1) {
		PDI_finalize();
		PC_tree_destroy(&conf);
		FAIL();
	}

	PDI_errhandler(std_handler); // changing to standart handler

	// ERROR
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 4;
		}
	}

	// change value of the context of the handler
	ctx.has_failed = 0;
	ctx.true_err_status = PDI_ERR_SYSTEM;
	ctx.true_errmsg
		= "Error while triggering event `error': System_error: Filename collision `decl_hdf5_test_collision_policy.h5': File already exists";

	std_handler = PDI_errhandler(local_errhandler); //changing err handler

	status = PDI_multi_expose("error", "array_data", array_data, PDI_OUT, NULL);

	EXPECT_EQ(ctx.has_failed, 1) << "Error expected but not reported (has_failed=" << ctx.has_failed << "), terminating \n";
	if (ctx.has_failed != 1) {
		PDI_finalize();
		PC_tree_destroy(&conf);
		FAIL();
	}

	PDI_errhandler(std_handler); // changing to standart handler

	PDI_multi_expose("read", "array_data", array_data, PDI_IN, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			if (array_data[i][j] != 3) {
				printf("array_data[%d][%d] = %d, should be: 4", i, j, array_data[i][j]);
				PDI_finalize();
				PC_tree_destroy(&conf);
				FAIL();
			}
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:            create_different_groups_with_regex
 *
 * Description:     test the creation of groups define by differents regex
 */
TEST_F(decl_hdf5_test, create_different_groups_with_regex)
{
	std::string filename = "decl_hdf5_test_create_different_groups_with_regex.h5";
	SetUp(filename);

	const char* CONFIG_YAML
		= "logging: trace                                                       \n"
		  "data:                                                                \n"
		  "  array_first: { size: [5, 10], type: array, subtype: int }          \n"
		  "  array_second: { size: [5, 10], type: array, subtype: int }         \n"
		  "  array_third: { size: [5, 10], type: array, subtype: int }          \n"
		  "metadata:                                                            \n"
		  "  index: int                                                         \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    file: decl_hdf5_test_create_different_groups_with_regex.h5       \n"
		  "    on_event: write_event                                            \n"
		  "    datasets:                                                        \n"
		  "      group/lion_array_data:                                         \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group[0-9]+/dog_array_data:                                    \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group_second.*/cat_array_data:                                 \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "    write:                                                           \n"
		  "      array_first:                                                    \n"
		  "        dataset: 'group${index}/dog_array_data'                      \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n"
		  "      array_second:                                                  \n"
		  "        dataset: group_second_TRY/cat_array_data                     \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n"
		  "      array_third:                                                   \n"
		  "        dataset: group/lion_array_data                               \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n";


	//PDI_write
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int index = 123;

	int array_01[5][10];
	int array_02[5][10];
	int array_03[5][10];

	int true_array_01[5][10];
	int true_array_02[5][10];
	int true_array_03[5][10];

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			array_01[i][j] = i * 10 + j;
			array_02[i][j] = (i * 10 + j) * 10;
			array_03[i][j] = (i * 10 + j) * 20;
		}
	}

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			true_array_01[i][j] = array_01[i][j];
			true_array_02[i][j] = array_02[i][j];
			true_array_03[i][j] = array_03[i][j];
		}
	}

	PDI_expose("index", &index, PDI_OUT);

	PDI_multi_expose("write_event", "array_first", array_01, PDI_OUT, "array_second", array_02, PDI_OUT, "array_third", array_03, PDI_OUT, NULL);

	PDI_finalize();
	PC_tree_destroy(&conf);

	CONFIG_YAML
		= "logging: trace                                                       \n"
		  "data:                                                                \n"
		  "  array_first: { size: [5, 10], type: array, subtype: int }          \n"
		  "  array_second: { size: [5, 10], type: array, subtype: int }         \n"
		  "  array_third: { size: [5, 10], type: array, subtype: int }          \n"
		  "metadata:                                                            \n"
		  "  index: int                                                         \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    file: decl_hdf5_test_create_different_groups_with_regex.h5     \n"
		  "    on_event: read_event                                             \n"
		  "    datasets:                                                        \n"
		  "      group/lion_array_data:                                         \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group[0-9]+/dog_array_data:                                    \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group_second.*/cat_array_data:                                 \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "    read:                                                            \n"
		  "      array_first:                                                   \n"
		  "        dataset: group${index}/dog_array_data                        \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n"
		  "      array_second:                                                  \n"
		  "        dataset: group_second_TRY/cat_array_data                     \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n"
		  "      array_third:                                                   \n"
		  "        dataset: group/lion_array_data                               \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n";

	//PDI_read
	conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			array_01[i][j] = 0;
			array_02[i][j] = 0;
			array_03[i][j] = 0;
		}
	}

	EXPECT_EQ(index, 123) << " The value have changed between wrting and reading";
	PDI_expose("index", &index, PDI_OUT);
	PDI_multi_expose("read_event", "array_first", array_01, PDI_IN, "array_second", array_02, PDI_IN, "array_third", array_03, PDI_IN, NULL);

	for (int i = 1; i < 4; i++) {
		for (int j = 1; j < 9; j++) {
			EXPECT_EQ(array_01[i][j], true_array_01[i][j])
				<< "Wrong value of array_01[" << i << "][" << j << "]: " << array_01[i][j] << " != " << true_array_01[i][j];
		}
	}

	for (int i = 1; i < 4; i++) {
		for (int j = 1; j < 9; j++) {
			EXPECT_EQ(array_02[i][j], true_array_02[i][j])
				<< "Wrong value of array_02[" << i << "][" << j << "]: " << array_02[i][j] << " != " << true_array_02[i][j];
		}
	}

	for (int i = 1; i < 4; i++) {
		for (int j = 1; j < 9; j++) {
			EXPECT_EQ(array_03[i][j], true_array_03[i][j])
				<< "Wrong value of array_03[" << i << "][" << j << "]: " << array_03[i][j] << " != " << true_array_03[i][j];
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:            create_groups_that_depend_on_index
 *
 * Description:     creating different groups defined by the same regex and depended on a index
 */
TEST_F(decl_hdf5_test, create_groups_that_depend_on_index)
{
	std::string filename = "decl_hdf5_test_create_groups_that_depend_on_index.h5";
	SetUp(filename);

	const char* CONFIG_YAML
		= "logging: trace                                                       \n"
		  "data:                                                                \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }           \n"
		  "metadata:                                                            \n"
		  "  index: int                                                         \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    file: decl_hdf5_test_create_groups_that_depend_on_index.h5       \n"
		  "    on_event: write_event                                            \n"
		  "    datasets:                                                        \n"
		  "      group[0-9]+/array_data:                                        \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "    write:                                                           \n"
		  "      array_data:                                                    \n"
		  "        dataset: 'group${index}/array_data'                          \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n";

	//PDI_write
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int index;
	int size_index = 2;
	int vec_index[size_index];
	for (int ii = 0; ii < size_index; ++ii) {
		vec_index[ii] = ii + 1;
	}

	int true_test_array[size_index][5][10];

	for (int ii = 0; ii < size_index; ++ii) {
		for (int i = 0; i < 5; i++) {
			for (int j = 0; j < 10; j++) {
				true_test_array[ii][i][j] = i * 10 + j + vec_index[ii] * 100;
			}
		}
	}

	int test_array[5][10];

	for (int ii = 0; ii < size_index; ++ii) {
		// define the array
		for (int i = 0; i < 5; i++) {
			for (int j = 0; j < 10; j++) {
				test_array[i][j] = true_test_array[ii][i][j];
			}
		}
		// change the value of index
		index = vec_index[ii];

		PDI_expose("index", &index, PDI_OUT);
		PDI_multi_expose("write_event", "array_data", test_array, PDI_OUT, NULL);
	}


	PDI_finalize();
	PC_tree_destroy(&conf);

	CONFIG_YAML
		= "logging: trace                                                       \n"
		  "data:                                                                \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }           \n"
		  "metadata:                                                            \n"
		  "  index: int                                                         \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    file: decl_hdf5_test_create_groups_that_depend_on_index.h5       \n"
		  "    on_event: read_event                                             \n"
		  "    read:                                                            \n"
		  "      array_data:                                                    \n"
		  "        dataset: group${index}/array_data                            \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n";

	//PDI_read
	conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	for (int ii = 0; ii < size_index; ++ii) {
		for (int i = 0; i < 5; i++) {
			for (int j = 0; j < 10; j++) {
				test_array[i][j] = 0;
			}
		}

		// change the value of index
		index = vec_index[ii];

		PDI_expose("index", &index, PDI_OUT);
		PDI_multi_expose("read_event", "array_data", test_array, PDI_IN, NULL);

		for (int i = 1; i < 4; i++) {
			for (int j = 1; j < 9; j++) {
				EXPECT_EQ(test_array[i][j], true_test_array[ii][i][j])
					<< "For dataset=group" << vec_index[ii] << "/array_data: Wrong value of test_array[" << i << "][" << j
					<< "]: " << test_array[i][j] << " != " << true_test_array[ii][i][j];
			}
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:            create_a_group_not_include_in_datasets_with_no_data_selection
 *
 * Description:     create a dataset with a group that is not include in the datasets section
 *                  with no section: dataset_selection
 */
TEST_F(decl_hdf5_test, create_a_group_not_include_in_datasets_with_no_data_selection)
{
	SetUp("decl_hdf5_test_create_a_group_not_include_in_datasets_with_no_data_selection.h5");

	const char* CONFIG_YAML
		= "logging: trace                                                       \n"
		  "data:                                                                \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }           \n"
		  "metadata:                                                            \n"
		  "  index: int                                                         \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    file: decl_hdf5_test_create_a_group_not_include_in_datasets_with_no_data_selection.h5   \n"
		  "    on_event: write_event                                            \n"
		  "    datasets:                                                        \n"
		  "      filename_array_data:                                           \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "    write:                                                           \n"
		  "      array_data:                                                    \n"
		  "        dataset: 'group${index}/array_data'                          \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int index = 123;
	int true_test_array[5][10];

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			true_test_array[i][j] = i * 10 + j;
		}
	}

	int test_array[5][10];

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = true_test_array[i][j];
		}
	}

	PDI_expose("index", &index, PDI_OUT);
	PDI_multi_expose("write_event", "array_data", test_array, PDI_OUT, NULL);

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.check_config_error_for_two_regex_found
 *
 * Description: check error message generated with a dataset where two regex are found
 */

TEST_F(decl_hdf5_test, check_config_error_for_two_regex_found)
{
	SetUp("decl_hdf5_test_two_regex_found.h5");

	int has_failed = 0;

	std::string true_errmsg
		= "Error while triggering event `write_event': Config_error in lines 44 - 45: Found `4' match(s) in the list of datasets "
		  "section for `group123/array_data'. Cannot choose the right element in datasets.\n"
		  "The elements that match group123/array_data are:\n"
		  " - group.*/array_data\n"
		  " - group1.*/array_data\n"
		  " - group12.*/array_data\n"
		  " - group[0-9]+/array_data\n"
		  "Attention: The elements are considered as a regex.";

	PDI_status_t true_status = PDI_ERR_CONFIG;
	context_check_error ctx{true_errmsg, true_status, has_failed};

	// defined local error handler
	PDI_errhandler_t local_errhandler;
	local_errhandler.func = succeed_on_failure;
	local_errhandler.context = static_cast<void*>(&ctx);

	const char* CONFIG_YAML
		= "logging:                                                             \n"
		  "  level: trace                                                       \n"
		  "  output:                                                            \n"
		  "    file: my_trace_logger_${index}.txt                               \n"
		  "    console: on                                                      \n"
		  "data:                                                                \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }           \n"
		  "metadata:                                                            \n"
		  "  index: int                                                         \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    logging:                                                         \n"
		  "      level: trace                                                   \n"
		  "      output:                                                        \n"
		  "        file: my_trace_logger_hdf5.txt                               \n"
		  "        console: on                                                  \n"
		  "    file: decl_hdf5_test_two_regex_found.h5                          \n"
		  "    on_event: write_event                                            \n"
		  "    datasets:                                                        \n"
		  "      group[0-9]+/array_data:                                        \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group.*/array_data:                                            \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group1.*/array_data:                                           \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group/.*/array_data:                                           \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group12.*/array_data:                                          \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "    write:                                                           \n"
		  "      array_data:                                                    \n"
		  "        dataset: 'group${index}/array_data'                          \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n";

	//PDI_write
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int index = 123;

	int test_array[5][10];

	int true_test_array[5][10];

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = i * 10 + j;
		}
	}

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			true_test_array[i][j] = test_array[i][j];
		}
	}

	PDI_status_t status_ok = PDI_expose("index", &index, PDI_OUT);

	// method to check without changing error handler
	EXPECT_TRUE(status_ok == PDI_OK) << "error: status = " << status_ok << " should be: 0 (PDI_OK) \n";
	std::string errmsg_index = PDI_errmsg();
	std::string empty_string = "";
	EXPECT_STREQ(errmsg_index.c_str(), empty_string.c_str()) << "Expect no error msg, we have this message:" << errmsg_index << "  \n";

	PDI_errhandler_t std_handler = PDI_errhandler(local_errhandler); //changing err handler

	PDI_status_t status = PDI_multi_expose("write_event", "array_data", test_array, PDI_OUT, NULL);

	PDI_errhandler(std_handler); // returning to standard PDI err_handler

	EXPECT_EQ(ctx.has_failed, 1) << "Error expected but not reported (has_failed=" << ctx.has_failed << "), terminating \n";

	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.check_config_error_for_no_regex_found
 *
 * Description:     check error when dataset_selection is given and no datasets is found  
 */
TEST_F(decl_hdf5_test, check_config_error_for_no_regex_found)
{
	SetUp("decl_hdf5_test_no_regex.h5");
	int has_failed = 0;
	std::string true_errmsg = "Error while triggering event `write_event': Config_error in lines 19 - 20: Dataset selection is invalid for implicit "
							  "dataset `group123/array_data'";
	PDI_status_t true_status = PDI_ERR_CONFIG;
	context_check_error ctx{true_errmsg, true_status, has_failed};

	// defined local error handler
	PDI_errhandler_t local_errhandler;
	local_errhandler.func = succeed_on_failure;
	local_errhandler.context = static_cast<void*>(&ctx);

	const char* CONFIG_YAML
		= "logging: trace                                                       \n"
		  "data:                                                                \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }           \n"
		  "metadata:                                                            \n"
		  "  index: int                                                         \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    file: decl_hdf5_test_no_regex.h5                                 \n"
		  "    on_event: write_event                                            \n"
		  "    datasets:                                                        \n"
		  "      group/array_data:                                              \n"
		  "        size: [3, 8]                                                 \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "    write:                                                           \n"
		  "      array_data:                                                    \n"
		  "        dataset: 'group${index}/array_data'                          \n"
		  "        dataset_selection:                                           \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [0, 0]                                              \n"
		  "        memory_selection:                                            \n"
		  "          size: [ 3, 8]                                              \n"
		  "          start: [1, 1]                                              \n";

	//PDI_write
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int index = 123;

	int test_array[5][10];

	int true_test_array[5][10];

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = i * 10 + j;
		}
	}

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			true_test_array[i][j] = test_array[i][j];
		}
	}

	PDI_expose("index", &index, PDI_OUT);

	PDI_errhandler_t std_handler = PDI_errhandler(local_errhandler); //changing err handler

	PDI_status_t status = PDI_multi_expose("write_event", "array_data", test_array, PDI_OUT, NULL);

	PDI_errhandler(std_handler); // returning to standard PDI err_handler


	EXPECT_EQ(ctx.has_failed, 1) << "Error expected but not reported (has_failed=" << ctx.has_failed << "), terminating \n";

	PDI_finalize();
	PC_tree_destroy(&conf);
}
