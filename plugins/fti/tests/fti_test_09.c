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
	
	long array_size = 64;
	int* array = malloc(sizeof(int) * array_size);
	
	int i;
	for (i = 0; i < array_size; i++) array[i] = i;
	PDI_expose("array_size", &array_size, PDI_OUT);
	PDI_multi_expose("ckpt",
	    "array", array, PDI_OUT,
	    NULL);
	    
	free(array);
	PDI_finalize();
	
	//recovery
	PDI_init(conf);
	long recover_size = 0;
	PDI_expose("fti_size", &recover_size, PDI_IN);
	assert(recover_size == sizeof(int) * array_size);
	
	array = malloc(recover_size);
	
	memset(array, 0, recover_size);
	PDI_expose("array_size", &array_size, PDI_OUT);
	PDI_multi_expose("recover",
	    "array", array, PDI_IN,
	    NULL);
	    
	for (i = 0; i < array_size; i++)
		assert(array[i] == i);
		
	free(array);
	PDI_finalize();
	MPI_Finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}
