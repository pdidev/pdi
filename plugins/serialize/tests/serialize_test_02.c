/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <assert.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	
	int sparse_int_array[8][4];
	double sparse_double_array[8][4];
	
	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 4; j++) {
			sparse_int_array[i][j] = i*4 + j;
			sparse_double_array[i][j] = 1.23 + i*4 + j;
		}
	}
	
	PDI_share("sparse_int_array", sparse_int_array, PDI_OUT);
	PDI_share("sparse_double_array", sparse_double_array, PDI_OUT);
	
	int* dense_int_array;
	double* dense_double_array;
	PDI_access("dense_int_array", (void**)&dense_int_array, PDI_IN);
	PDI_access("dense_double_array", (void**)&dense_double_array, PDI_IN);
	
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 2; j++) {
			printf("s_i_a[%d][%d] = %d ?= d_i_a[%d][%d] = %d\n", i+2, j+1, sparse_int_array[i+2][j+1], i, j, dense_int_array[i*2 + j]);
			assert(sparse_int_array[i+2][j+1] == dense_int_array[i*2 + j]);
			printf("s_d_a[%d][%d] = %f ?= d_d_a[%d][%d] = %f\n", i+2, j+1, sparse_double_array[i+2][j+1], i, j, dense_double_array[i*2 + j]);
			assert(sparse_double_array[i+2][j+1] == dense_double_array[i*2 + j]);
		}
	}
	
	PDI_release("dense_int_array");
	PDI_release("dense_double_array");
	
	PDI_reclaim("sparse_int_array");
	PDI_reclaim("sparse_double_array");
	
	PDI_finalize();
	return 0;
}
