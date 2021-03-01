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
	    "  dset_attr: {type: array, subtype: int, size: 4}                       \n"
	    "  size_attr: int                                                        \n"
	    "plugins:                                                                \n"
	    "  decl_hdf5:                                                            \n"
	    "    file: decl_hdf5_test_12.h5                                          \n"
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
	
	return 0;
}

int PDI_read()
{
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
	    "    file: decl_hdf5_test_12.h5                                          \n"
	    "    on_event: \"read\"                                                  \n"
	    "    read: [array_data, array_data#dset_attr, array_data#size_attr]     \n"
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
	
	int size_attr = 0;
	PDI_multi_expose("read",
	    "size_attr", &size_attr, PDI_IN,
	    "array_data", test_array, PDI_IN,
	    "dset_attr", dset_attr, PDI_IN, NULL);
	    
	if (size_attr != 10) {
		printf("size_attr invalid value: %d (should be: 10)\n", size_attr);
		return 1;
	}
	
	for (int i = 0; i < 4; i++) {
		if (dset_attr[i] != i) {
			printf("dset_attr[%d] invalid value: %d (should be: %d)\n", i, dset_attr[i], i);
			return 1;
		}
	}
	
	for (int i = 0; i < 10; i++) {
		if (test_array[i] != i) {
			printf("test_array[%d] invalid value: %d (should be: %d)\n", i, test_array[i], i);
			return 1;
		}
	}
	
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
