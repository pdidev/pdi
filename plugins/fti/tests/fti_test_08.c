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

#include <assert.h>
#include <mpi.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_path(argv[1]);
	PDI_init(conf);
	
	int test = 10;
	int array[10];
	int array2D[3][3];
	
	PDI_multi_expose("ckpt",
	    "test", &test, PDI_OUT,
	    "array", array, PDI_OUT,
	    "array2D", array2D, PDI_OUT,
	    NULL);
	    
	int status;
	PDI_expose("fti_status", &status, PDI_IN);
	assert(status == 0);
	
	int test_size, array_size, array2D_size;
	PDI_expose("test_size", &test_size, PDI_IN);
	PDI_expose("array_size", &array_size, PDI_IN);
	PDI_expose("array2D_size", &array2D_size, PDI_IN);
	
	assert(test_size == sizeof(test));
	assert(array_size == sizeof(array));
	assert(array2D_size == sizeof(array2D));
	
	test_size = array_size = array2D_size = 0;
	PDI_finalize();
	
	//recovery
	PDI_init(conf);
	
	PDI_expose("test_size", &test_size, PDI_IN);
	PDI_expose("array_size", &array_size, PDI_IN);
	PDI_expose("array2D_size", &array2D_size, PDI_IN);
	
	assert(test_size == sizeof(test));
	assert(array_size == sizeof(array));
	assert(array2D_size == sizeof(array2D));
	
	PDI_finalize();
	MPI_Finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}
