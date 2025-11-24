/*******************************************************************************
 * Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <pdi.h>

constexpr char pdi_config[] = R"(
logging: trace
metadata:
  pb_size: int
  input: int
  chunk: int
data:
  int_scalar: int
  int_array: {type: array, subtype: int, size: $pb_size}
  int_matrix: {type: array, subtype: int, size: ['$pb_size', '$pb_size']}
plugins:
  decl_netcdf:
    - file: 'test_deflate_0.nc'
      variables:
        int_scalar: int
        int_array:
          type: array
          subtype: int
          size: $pb_size
          dimensions: ['time']
        int_matrix:
          type: array
          subtype: int
          size: ['$pb_size', '$pb_size']
          dimensions: ['col', 'row']
      when: '${input}=0'
      write: [int_scalar, int_array, int_matrix]  
  
    - file: 'test_deflate_6.nc'
      variables:
        int_scalar: int
        int_array:
          type: array
          subtype: int
          size: $pb_size
          dimensions: ['time']
          deflate: 6
        int_matrix:
          type: array
          subtype: int
          size: ['$pb_size', '$pb_size']
          dimensions: ['col', 'row']
          deflate: 6
      when: '${input}=0'
      write: [int_scalar, int_array, int_matrix]
    
    - file: 'test_deflate_9.nc'
      variables:
        int_scalar: int
        int_array:
          type: array
          subtype: int
          size: $pb_size
          dimensions: ['time']
          deflate: 9
          chunking: $chunk
        int_matrix:
          type: array
          subtype: int
          size: ['$pb_size', '$pb_size']
          dimensions: ['col', 'row']
          deflate: 9
          chunking: ['$chunk', '$chunk']
      when: '${input}=0'
      write: [int_scalar, int_array, int_matrix]

    - file: 'test_deflate_6.nc'
      when: '${input}=1'
      read: [int_scalar, int_array, int_matrix]
)";

// Tests simple write and read of scalar and array depending on event
int main(int argc, char* argv[])
{
	PDI_init(PC_parse_string(pdi_config));

	// init data
	int input = 0;
	int int_scalar = 42;
	int N = 1000;
	int chunk = 1000;
	int* int_array = new int[N];
	int* int_matrix = new int[N * N];

	// write data
	for (int i = 0; i < N; i++) {
		int_array[i] = i;
	}

	for (int i = 0; i < N * N; i++) {
		int_matrix[i] = i;
	}

	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("pb_size", &N, PDI_OUT);
	PDI_expose("chunk", &chunk, PDI_OUT);

	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array, PDI_OUT);
	PDI_expose("int_matrix", int_matrix, PDI_OUT);

	// read data
	input = 1;
	int_scalar = 0;
	for (int i = 0; i < N; i++) {
		int_array[i] = 0;
	}
	for (int i = 0; i < N * N; i++) {
		int_matrix[i] = 0;
	}

	PDI_expose("input", &input, PDI_OUT);

	PDI_expose("int_scalar", &int_scalar, PDI_IN);
	PDI_expose("int_array", int_array, PDI_IN);
	PDI_expose("int_matrix", int_matrix, PDI_IN);

	// verify
	int status = 0;
	if (int_scalar != 42) {
		printf("int_scalar = %d != %d\n", int_scalar, 42);
		status = 1;
	}
	for (int i = 0; i < N; i++) {
		if (int_array[i] != i) {
			printf("int_array[%d] = %d != %d\n", i, int_array[i], i);
			status = 1;
		}
	}

	for (int i = 0; i < N * N; i++) {
		if (int_matrix[i] != i) {
			printf("int_matrix[%d][%d] = %d != %d\n", i / N, i % N, int_matrix[i], i);
			status = 1;
		}
	}

	delete[] int_array;
	delete[] int_matrix;

	PDI_finalize();
	return status;
}
