/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <unistd.h>
#include <pdi.h>
#include <gtest/gtest.h>
//#include <hdf5.h>

/*
 * Name:                decl_hdf5_test.01
 *
 * Description:         Metatadata export using filename
 */
TEST(decl_hdf5_test, 01) { 

	const char* CONFIG_YAML =
		"logging: trace                \n"
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
		"        meta2: ~              \n"
		;

	int value[5] = {5,4,3,2,1};
	double test_var = 0;
	
	remove("5.h5");
	
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	PDI_multi_expose("testing",
	    "meta0",&value[0], PDI_OUT,
	    "meta1",&value[0], PDI_OUT,
	    "meta2",&value[1], PDI_OUT,
	    "meta3",&value[2], PDI_OUT,
	    "meta4",&value[3], PDI_OUT,
	    "test_var",&test_var, PDI_OUT,
	    NULL);
	    
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	FILE* fp = fopen("5.h5", "r");
	EXPECT_NE (fp, nullptr) << "File not found. \n";
	fclose(fp);
}

/*
 * Name:                decl_hdf5_test.02
 *
 * Description:         outer record
 */
TEST(decl_hdf5_test, 02) { 

	const char* CONFIG_YAML =
	"logging: trace              \n"
	"metadata:                   \n"
	"  input: int                \n"
	"data:                       \n"
	"  outer_record:             \n"
	"    type: struct            \n"
	"    members:                \n"
	"      - id: int             \n"
	"      - value:              \n"
	"          type: array       \n"
	"          size: [2, 2]      \n"
	"          subtype:          \n"
	"            type: struct    \n"
	"            members:        \n"
	"              - x: int      \n"
	"              - y: int      \n"
	"              - z: int      \n"
	"plugins:                    \n"
	"  decl_hdf5:                \n"
	"    - file: test_02.h5      \n"
	"      write: [outer_record] \n"
	"      when: $input=0        \n"
	"    - file: test_02.h5      \n"
	"      read: [outer_record]  \n"
	"      when: $input=1        \n"
	;

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
			outer_record.value[i].x = -1*i;
			outer_record.value[i].y = 2*i;
			outer_record.value[i].z = 3*i;
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
			EXPECT_EQ(outer_record.value[i].x, -1*i);
			EXPECT_EQ(outer_record.value[i].y, 2*i);
			EXPECT_EQ(outer_record.value[i].z, 3*i);
		}
		PDI_finalize();
		PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.03
 *
 * Description:         record with dataset_selection and memory_selection
 */
TEST(decl_hdf5_test, 03) {

	const char* CONFIG_YAML =
	"logging: trace                             \n"
	"metadata:                                  \n"
	"  input: int                               \n"
	"data:                                      \n"
	"  array_of_record:                         \n"
	"    type: array                            \n"
	"    size: 4                                \n"
	"    subtype:                               \n"
	"      type: struct                         \n"
	"      members:                             \n"
	"        - id: int                          \n"
	"        - value:                           \n"
	"            type: array                    \n"
	"            subtype: int                   \n"
	"            size: [4, 4]                   \n"
	"  array_of_record_read:                    \n"
	"    type: array                            \n"
	"    size: 2                                \n"
	"    subtype:                               \n"
	"      type: struct                         \n"
	"      members:                             \n"
	"        - id: int                          \n"
	"        - value:                           \n"
	"            type: array                    \n"
	"            subtype: int                   \n"
	"            size: [4, 4]                   \n"
	"plugins:                                   \n"
	"  decl_hdf5:                               \n"
	"    - file: test_03.h5                     \n"
	"      when: $input=0                       \n"
	"      datasets:                            \n"
	"        data_array:                        \n"
	"          type: array                      \n"
	"          size: 2                          \n"
	"          subtype:                         \n"
	"            type: struct                   \n"
	"            members:                       \n"
	"              - id: int                    \n"
	"              - value:                     \n"
	"                  type: array              \n"
	"                  subtype: int             \n"
	"                  size: [4, 4]             \n"
	"      write:                               \n"
	"        array_of_record:                   \n"
	"          dataset: data_array              \n"
	"          memory_selection:                \n"
	"            size: 2                        \n"
	"            start: 1                       \n"
	"          dataset_selection:               \n"
	"            size: 2                        \n"
	"    - file: test_03.h5                     \n"
	"      when: $input=1                       \n"
	"      read: [array_of_record_read]         \n"
	;

	struct Record {
		int id;
		int value[16];
	};

	struct Record rec[4];
	// init data
	for (int i = 0; i < 4; i++) {
		rec[i].id = i;
		for (int j = 0; j < 16; j++) {
			rec[i].value[j] = j + i*16;
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
			EXPECT_EQ(rec[i].value[j], j + i*16);
		}
	}
	PDI_finalize();
	PC_tree_destroy(&conf);
}

/*
 * Name:                decl_hdf5_test.04
 *
 * Description:         attribute
 */
TEST(decl_hdf5_test, 04) { 

//PDI_write

	const char* CONFIG_YAML =
		"logging: trace                                                          \n"
		"metadata:                                                               \n"
		"  array_size: int                                                       \n"
		"data:                                                                   \n"
		"  array_data: { size: $array_size, type: array, subtype: int }          \n"
		"  group_attr: float                                                     \n"
		"  dset_attr: {type: array, subtype: int, size: 4}                       \n"
		"plugins:                                                                \n"
		"  decl_hdf5:                                                            \n"
		"    file: decl_hdf5_test_04.h5                                          \n"
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
		"        attribute: data#group_attr_name                                 \n"
		;
		
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

	CONFIG_YAML =
		"logging: trace                                                          \n"
		"metadata:                                                               \n"
		"  array_size: int                                                       \n"
		"data:                                                                   \n"
		"  array_data: { size: 10, type: array, subtype: int }                   \n"
		"  dset_attr: {type: array, subtype: int, size: 4}                       \n"
		"  group_attr: float                                                     \n"
		"  expr_attr: int                                                        \n"
		"plugins:                                                                \n"
		"  decl_hdf5:                                                            \n"
		"    file: decl_hdf5_test_04.h5                                          \n"
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
		"        attribute: data#group_attr_name                                 \n"
		;
	
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
		EXPECT_EQ(test_array[i], i ) << "test_array[" << i << "] invalid value: " << test_array[i] << "(should be: " << i << ")";
	}
	EXPECT_EQ(expr_attr, 10) << "expr_attr invalid value: " << expr_attr << " (should be: 10)";
	PDI_reclaim("expr_attr");
	
	PDI_finalize();
	PC_tree_destroy(&conf);

}

/*
 * Name:                decl_hdf5_test.05
 *
 * Description:         attribute testing
 */
TEST(decl_hdf5_test, 05) { 

//PDI_write
	const char* CONFIG_YAML =
		"logging: trace                                                          \n"
		"metadata:                                                               \n"
		"  array_size: int                                                       \n"
		"data:                                                                   \n"
		"  array_data: { size: $array_size, type: array, subtype: int }          \n"
		"  dset_attr: {type: array, subtype: int, size: 4}                       \n"
		"  size_attr: int                                                        \n"
		"plugins:                                                                \n"
		"  decl_hdf5:                                                            \n"
		"    file: decl_hdf5_test_05.h5                                          \n"
		"    on_event: \"write\"                                                 \n"
		"    write: [array_data, array_data#dset_attr, array_data#size_attr]    \n"
		;
		
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
	PDI_multi_expose("write",
		"size_attr", &array_size, PDI_OUT,
		"array_data", test_array, PDI_OUT,
		"dset_attr", dset_attr, PDI_OUT, NULL);
		
	PDI_finalize();
	PC_tree_destroy(&conf);

//PDI_read
	CONFIG_YAML =
		"logging: trace                                                          \n"
		"metadata:                                                               \n"
		"  array_size: int                                                       \n"
		"data:                                                                   \n"
		"  array_data: { size: $array_size, type: array, subtype: int }          \n"
		"  dset_attr: {type: array, subtype: int, size: 4}                       \n"
		"  size_attr: int                                                        \n"
		"plugins:                                                                \n"
		"  decl_hdf5:                                                            \n"
		"    file: decl_hdf5_test_05.h5                                          \n"
		"    on_event: \"read\"                                                  \n"
		"    read: [array_data, array_data#dset_attr, array_data#size_attr]     \n"
		;
		
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
	PDI_multi_expose("read",
		"size_attr", &size_attr, PDI_IN,
		"array_data", test_array, PDI_IN,
		"dset_attr", dset_attr, PDI_IN, NULL);
		
	EXPECT_EQ(size_attr, 10) << "size_attr invalid value: " << size_attr <<" (should be: 10)";
	
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
TEST(decl_hdf5_test, 06) { 

//PDI_write
	const char* CONFIG_YAML =
		"logging: trace                                                          \n"
		"data:                                                                   \n"
		"  array_data: { size: 10, type: array, subtype: int }                   \n"
		"  matrix_data: { size: [10, 10], type: array, subtype: float }          \n"
		"plugins:                                                                \n"
		"  decl_hdf5:                                                            \n"
		"    file: decl_hdf5_test_06.h5                                          \n"
		"    write: [array_data, matrix_data]                                    \n"
		;
		
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
			matrix_data[i][j] = 10*i + j;
		}
	}
	PDI_expose("matrix_data", matrix_data, PDI_OUT);
	
	PDI_finalize();
	PC_tree_destroy(&conf);

//PDI_read
	CONFIG_YAML =
		"logging: trace                                                          \n"
		"metadata:                                                               \n"
		"  input: int                                                            \n"
		"data:                                                                   \n"
		"  array_data_size: int64                                                \n"
		"  matrix_data_size: { size: 2, type: array, subtype: int64 }            \n"
		"plugins:                                                                \n"
		"  decl_hdf5:                                                            \n"
		"    - file: decl_hdf5_test_06.h5                                        \n"
		"      when: $input                                                      \n"
		"      read:                                                             \n"
		"        array_data_size:                                                \n"
		"          size_of: array_data                                           \n"
		"        matrix_data_size:                                               \n"
		"          size_of: matrix_data                                          \n"
		"    - file: decl_hdf5_test_06.h5                                        \n"
		"      on_event: \"read_size\"                                           \n"
		"      read:                                                             \n"
		"        array_data_size:                                                \n"
		"          size_of: array_data                                           \n"
		"        matrix_data_size:                                               \n"
		"          size_of: matrix_data                                          \n"
		;
		
	conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int input = 1;
	PDI_expose("input", &input, PDI_OUT);
	
	long array_size = 0;
	long matrix_size[2] = {0, 0};
	PDI_expose("array_data_size", &array_size, PDI_IN);
	PDI_expose("matrix_data_size", matrix_size, PDI_IN);
	
	EXPECT_EQ(array_size, 10) << "array_size invalid value: " << array_size << " (should be: 10)";
	EXPECT_TRUE(matrix_size[0] == 10 || matrix_size[1] == 10) << "matrix_size invalid value: [" << matrix_size[0] << ", " <<matrix_size[1] << "] (should be: [10, 10]";
	
	// now with event
	input = 0;
	PDI_expose("input", &input, PDI_OUT);
	
	array_size = 0;
	matrix_size[0] = 0;
	matrix_size[1] = 0;
	PDI_multi_expose("read_size",
		"array_data_size", &array_size, PDI_IN,
		"matrix_data_size", matrix_size, PDI_IN,
		NULL);
		
	EXPECT_EQ(array_size, 10) << "array_size invalid value: " << array_size << " (should be: 10)";
	EXPECT_TRUE(matrix_size[0] == 10 || matrix_size[1] == 10) << "matrix_size invalid value: [" << matrix_size[0] << ", " <<matrix_size[1] << "] (should be: [10, 10]";
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
}

/*
 * Name:                decl_hdf5_test.07
 *
 * Description:         different dimension of data and dataset
 */
TEST(decl_hdf5_test, 07) { 

	//PDI_write
	const char* CONFIG_YAML =
		"logging: trace                                                 \n"
		"data:                                                          \n"
		"  scalar_data: double                                          \n"
		"  array_data: { size: [8, 8], type: array, subtype: int }      \n"
		"plugins:                                                       \n"
		"  decl_hdf5:                                                   \n"
		"    file: decl_hdf5_test_07.h5                                 \n"
		"    datasets:                                                  \n"
		"      scalar_data: {type: array, subtype: double, size: 1}     \n"
		"      array_data: {type: array, subtype: int, size: [4, 4, 4]} \n"
		"    write: [scalar_data, array_data]                           \n"
		;
		
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
	CONFIG_YAML =
		"logging: trace                                                 \n"
		"data:                                                          \n"
		"  scalar_data: double                                          \n"
		"  array_data: { size: [8, 8], type: array, subtype: int }      \n"
		"plugins:                                                       \n"
		"  decl_hdf5:                                                   \n"
		"    file: decl_hdf5_test_07.h5                                 \n"
		"    datasets:                                                  \n"
		"      scalar_data: {type: array, subtype: double, size: 1}     \n"
		"      array_data: {type: array, subtype: int, size: [4, 4, 4]} \n"
		"    read: [scalar_data, array_data]                            \n"
		;
		
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
			EXPECT_EQ(array_data[i][j], i * 8 + j) << "Wrong value of array_data[" << i << "][" << j << "]: " << array_data[i][j] << " != " << i * 8 + j;
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
 * Description:         colission policy
 */
 TEST(decl_hdf5_test, 08) { 

	const char* CONFIG_YAML =
	    "logging: trace                                                 \n"
	    "data:                                                          \n"
	    "  scalar_data: int                                             \n"
	    "  array_data: { size: [4, 4], type: array, subtype: int }      \n"
	    "plugins:                                                       \n"
	    "  decl_hdf5:                                                   \n"
	    "    - file: decl_hdf5_test_08.h5                               \n"
	    "      on_event: init                                           \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [array_data]                                      \n"
	    "    - file: decl_hdf5_test_08.h5                               \n"
	    "      collision_policy: skip                                   \n"
	    "      on_event: skip                                           \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [array_data]                                      \n"
	    "    - file: decl_hdf5_test_08.h5                               \n"
	    "      collision_policy: write_into                             \n"
	    "      on_event: write_into                                     \n"
	    "      datasets:                                                \n"
	    "        scalar_data: int                                       \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [scalar_data, array_data]                         \n"
	    "    - file: decl_hdf5_test_08.h5                               \n"
	    "      collision_policy: replace                                \n"
	    "      on_event: replace                                        \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [array_data]                                      \n"
	    "    - file: decl_hdf5_test_08.h5                               \n"
	    "      collision_policy: write_into                             \n"
	    "      on_event: append                                         \n"
	    "      datasets:                                                \n"
	    "        scalar_data: int                                       \n"
	    "      write:                                                   \n"
	    "        scalar_data:                                           \n"
	    "          collision_policy: error                              \n"
	    "    - file: decl_hdf5_test_08.h5                               \n"
	    "      collision_policy: error                                  \n"
	    "      on_event: error                                          \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [array_data]                                      \n"
	    "    - file: decl_hdf5_test_08.h5                               \n"
	    "      on_event: read                                           \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      read: [array_data]                                       \n"
	    "    - file: decl_hdf5_test_08.h5                               \n"
	    "      on_event: read_scalar                                    \n"
	    "      datasets:                                                \n"
	    "        scalar_data: int                                       \n"
	    "      read: [scalar_data]                                      \n"
	    ;
	    
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
	PDI_multi_expose("write_into",
	    "scalar_data", &scalar_data, PDI_OUT,
	    "array_data", array_data, PDI_OUT,
	    NULL);
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
	PDI_errhandler(PDI_NULL_HANDLER);
	PDI_status_t status = PDI_multi_expose("read_scalar", "scalar_data", &scalar_data, PDI_IN, NULL);
	if (status == PDI_OK) {
		printf("replace: status = %d, should not be: 0 (PDI_OK)", status);
		PDI_finalize();
		PC_tree_destroy(&conf);
		FAIL();
	}
	PDI_errhandler(PDI_ASSERT_HANDLER);
	
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
	PDI_errhandler(PDI_NULL_HANDLER);
	status = PDI_multi_expose("append", "scalar_data", &scalar_data, PDI_OUT, NULL);
	if (status == PDI_OK) {
		printf("append: status = %d, should not be: 0 (PDI_OK)", status);
		PDI_finalize();
		PC_tree_destroy(&conf);
		FAIL();
	}
	PDI_errhandler(PDI_ASSERT_HANDLER);
	
	// ERROR
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 4;
		}
	}
	PDI_errhandler(PDI_NULL_HANDLER);
	status = PDI_multi_expose("error", "array_data", array_data, PDI_OUT, NULL);
	if (status == PDI_OK) {
		printf("error: status = %d, should not be: 0 (PDI_OK)", status);
		PDI_finalize();
		PC_tree_destroy(&conf);
		FAIL();
	}
	
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
	PDI_errhandler(PDI_ASSERT_HANDLER);
	
	PDI_finalize();
	PC_tree_destroy(&conf);

 }