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


// Tests variable selection on write and read
int main(int argc, char* argv[])
{	
	PDI_init(PC_parse_path(argv[1]));

    // init data
    int int_matrix_0[4][4];
    int int_matrix_1[4][4];
    int int_matrix_2[4][4];
    int int_matrix_3[4][4];
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            int_matrix_0[i][j] = i*4 + j;
        }
    }
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            int_matrix_1[i][j] = 100 + i*4 + j;
        }
    }
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            int_matrix_2[i][j] = 200 + i*4 + j;
        }
    }
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            int_matrix_3[i][j] = 300 + i*4 + j;
        }
    }

    // write data
    PDI_multi_expose("write",
                     "int_submatrix_0", int_matrix_0, PDI_OUT,
                     "int_submatrix_1", int_matrix_1, PDI_OUT,
                     "int_submatrix_2", int_matrix_2, PDI_OUT,
                     "int_submatrix_3", int_matrix_3, PDI_OUT,
                     NULL);

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

    PDI_multi_expose("read",
                     "int_submatrix_left", int_matrix_left, PDI_IN,
                     "int_submatrix_right", int_matrix_right, PDI_IN,
                     NULL);

    // verify
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            printf("[%d][%d] %d ?= %d\n", i, j, int_matrix_left[i][j], i*4 + j);
            assert(int_matrix_left[i][j] == i*4 + j);
        }
    }
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            printf("[%d][%d] %d ?= %d\n", i, j, int_matrix_right[i][j], 100 + i*4 + j);
            assert(int_matrix_right[i][j] == 100 + i*4 + j);
        }
    }
    
    for (int i = 4; i < 8; i++) {
        for (int j = 0; j < 4; j++) {
            printf("[%d][%d] %d ?= %d\n", i, j, int_matrix_left[i][j], 200 + (i-4)*4 + j);
            assert(int_matrix_left[i][j] == 200 + (i-4)*4 + j);
        }
    }

    for (int i = 4; i < 8; i++) {
        for (int j = 0; j < 4; j++) {
            printf("[%d][%d] %d ?= %d\n", i, j, int_matrix_right[i][j], 300 + (i-4)*4 + j);
            assert(int_matrix_right[i][j] == 300 + (i-4)*4 + j);
        }
    }
    
    PDI_finalize();
    return 0;
}
