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

// Tests simple write and read of scalar and array depending on `input' metadata
int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	
	// init data
	int input = 0;
	int int_scalar = 42;
	int int_array[32];
	for (int i = 0; i < 32; i++) {
		int_array[i] = i;
	}
	
	// write data
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array, PDI_OUT);
	
	// zero data
	int_scalar = 0;
	for (int i = 0; i < 32; i++) {
		int_array[i] = 0;
	}
	
	// read data
	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_IN);
	PDI_expose("int_array", int_array, PDI_IN);
	
	// verify
	printf("%d ?= %d\n", int_scalar, 42);
	assert(int_scalar == 42);
	for (int i = 0; i < 32; i++) {
		printf("%d ?= %d\n", int_array[i], i);
		assert(int_array[i] == i);
	}
	
	PDI_finalize();
	return 0;
}
