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
	
	int int_scalar = 42;
	double double_scalar = 42.42;
	
	PDI_share("int_scalar", &int_scalar, PDI_OUT);
	PDI_share("double_scalar", &double_scalar, PDI_OUT);
	
	int* int_serialized;
	double* double_serialized;
	PDI_access("int_serialized", (void**)&int_serialized, PDI_IN);
	PDI_access("double_serialized", (void**)&double_serialized, PDI_IN);
	
	printf("%d ?= %d\n", int_scalar, *int_serialized);
	assert(int_scalar == *int_serialized);
	
	printf("%f ?= %f\n", double_scalar, *double_serialized);
	assert(double_scalar == *double_serialized);
	
	PDI_release("int_serialized");
	PDI_release("double_serialized");
	
	PDI_reclaim("int_scalar");
	PDI_reclaim("double_scalar");
	
	PDI_errhandler(PDI_NULL_HANDLER);
	PDI_status_t status = PDI_access("int_serialized", (void**)&int_serialized, PDI_IN);
	if (status == PDI_OK) {
		printf("Serialized data was not released");
		exit(1);
	}
	status = PDI_access("double_serialized", (void**)&double_serialized, PDI_IN);
	if (status == PDI_OK) {
		printf("Serialized data was not released");
		exit(1);
	}
	
	PDI_finalize();
	return 0;
}
