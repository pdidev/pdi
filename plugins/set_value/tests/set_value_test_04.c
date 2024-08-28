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

#include <paraconf.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

	// int
	int* size;
	PDI_access("array_size", (void**)&size, PDI_IN);
	assert(*size == 6);
	PDI_release("array_size");

	int* int_array;
	PDI_access("array_metadata", (void**)&int_array, PDI_IN);
	for (int i = 0; i < 4; i++) {
		assert(int_array[i] == i + 1);
	}
	PDI_release("array_metadata");

	PDI_event("update_array");

	PDI_access("array_metadata", (void**)&int_array, PDI_IN);
	for (int i = 0; i < 6; i++) {
		assert(int_array[i] == 9 - i);
	}
	PDI_release("array_metadata");

	PDI_access("array_size", (void**)&size, PDI_IN);
	assert(*size == 0);
	PDI_reclaim("array_size");

	free(size);

	PDI_finalize();
}
