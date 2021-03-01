/*******************************************************************************
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi.h>
#include <unistd.h>

int PDI_write()
{
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
	    "    file: decl_hdf5_test_11.h5                                          \n"
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
	
	return 0;
}

int PDI_read()
{
	const char* CONFIG_YAML =
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
	    "    file: decl_hdf5_test_11.h5                                          \n"
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
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int array_size = 10;
	int test_array[10];
	for (int i = 0; i < 10; i++) {
		test_array[i] = 0;
	}
	PDI_expose("array_size", &array_size, PDI_OUT);
	
	float group_attr = 0.0f;
	int dset_attr[4];
	for (int i = 0; i < 4; i++) {
		dset_attr[i] = 0;
	}
	
	PDI_expose("group_attr", &group_attr, PDI_IN);
	if (group_attr != 1.2345f) {
		printf("group_attr invalid value: %f (should be: 1.2345)\n", group_attr);
		return 1;
	}
	
	PDI_expose("dset_attr", dset_attr, PDI_IN);
	for (int i = 0; i < 4; i++) {
		if (dset_attr[i] != i) {
			printf("dset_attr[%d] invalid value: %d (should be: %d)\n", i, dset_attr[i], i);
			return 1;
		}
	}
	
	int expr_attr = 0;
	PDI_share("expr_attr", &expr_attr, PDI_IN);
	PDI_expose("array_data", test_array, PDI_IN);
	for (int i = 0; i < 10; i++) {
		if (test_array[i] != i) {
			printf("test_array[%d] invalid value: %d (should be: %d)\n", i, test_array[i], i);
			return 1;
		}
	}
	if (expr_attr != 10) {
		printf("expr_attr invalid value: %d (should be: 10)\n", expr_attr);
		return 1;
	}
	PDI_reclaim("expr_attr");
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int main()
{
	int status = PDI_write();
	if (status !=0) {
		return status;
	}
	status = PDI_read();
	if (status !=0) {
		return status;
	}
	
	return 0;
}
