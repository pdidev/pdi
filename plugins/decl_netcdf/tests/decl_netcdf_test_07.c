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

#include <assert.h>
#include <pdi.h>


// Tests infinite dimension
int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	
	int int_matrix[8][8];
	for (int iter = 0; iter < 32; iter++) {
		// init data
		for (int i = 0; i < 8; i++) {
			for (int j = 0; j < 8; j++) {
				int_matrix[i][j] = iter*100 + i*8 + j;
			}
		}
		
		// write data
		PDI_multi_expose("write",
		    "iter", &iter, PDI_OUT,
		    "int_matrix", int_matrix, PDI_OUT,
		    NULL);
	}
	
	for (int iter = 0; iter < 32; iter++) {
		// read data
		PDI_multi_expose("read",
		    "iter", &iter, PDI_OUT,
		    "int_matrix", int_matrix, PDI_IN,
		    NULL);
		    
		// verify
		for (int i = 0; i < 8; i++) {
			for (int j = 0; j < 8; j++) {
				printf("[%d][%d] %d ?= %d\n", i, j, int_matrix[i][j], iter*100 + i*8 + j);
				assert(int_matrix[i][j] == iter*100 + i*8 + j);
			}
		}
	}
	
	PDI_finalize();
	return 0;
}
