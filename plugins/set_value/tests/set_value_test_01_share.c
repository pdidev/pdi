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
#include <stdio.h>

#include <paraconf.h>
#include <pdi.h>

struct Record_data {
	int scalar_data;
	int array_data[4];
} typedef Record_data;

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	Record_data* record;
	PDI_access("record_data", (void**)&record, PDI_IN);
	printf("Scalar: %d\n", record->scalar_data);
	assert(record->scalar_data == 4);
	for (int i = 0; i < 4; i++) {
		printf("Array[%d] = %d\n", i, record->array_data[i]);
		assert(record->array_data[i] == i + 2);
	}
	PDI_release("record_data");
	PDI_finalize();
}
