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

#include <paraconf.h>
#include <pdi.h>

void expose_int_array()
{
	int sparse_array[1000]; //buffer: 10 x 10 x 10; data: 3 x 4 x 5; start: (1, 2, 3)
	int* dense_array; //buffer: 3 x 4 x 5
	
	for (int i = 0; i < 1000; i++) {
		sparse_array[i] = i;
	}
	
	// metadata expose creates a dense copy inside PDI
	PDI_expose("my_array", sparse_array, PDI_OUT);
	PDI_access("my_array", (void**)&dense_array, PDI_IN);
	
	for (int i = 0; i < 60; i++) {
		assert(dense_array[i] == (i/20 + 1)*100 + (i/5 % 4 + 2)*10 + i%5 + 3);
	}
}

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	assert(argc == 2 && "Needs 1 single arg: config file");
	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_init(conf, &world);
	
	expose_int_array();
	
	PDI_finalize();
	MPI_Finalize();
	return 0;
}
