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

#include "serialize_test.h"

struct Record_data {
	int a;
	int* b;
} typedef Record_data;

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	int input = 0;
	PDI_expose("input", &input, PDI_OUT);
	
	int scalar_data = 42;
	PDI_expose("scalar_data", &scalar_data, PDI_OUT);
	int array_data[8];
	for (int i = 0; i < 8; i++) {
		array_data[i] = 42 + i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);
	
	Record_data record_data;
	record_data.a = 50;
	int b = 51;
	record_data.b = &b;
	PDI_expose("record_data", &record_data, PDI_OUT);
	
	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	
	int scalar_data_read;
	PDI_expose("scalar_data", &scalar_data_read, PDI_IN);
	printf("%d ?== %d\n", scalar_data, scalar_data_read);
	assert(scalar_data == scalar_data_read);
	
	int array_data_read[8];
	PDI_expose("array_data", array_data_read, PDI_IN);
	for (int i = 2; i < 6; i++) {
		printf("[%d] %d ?== %d\n", i, array_data[i], array_data_read[i]);
		assert(array_data[i] == array_data_read[i]);
	}
	
	int b_read;
	Record_data record_data_read;
	record_data_read.b = &b_read;
	PDI_expose("record_data", &record_data_read, PDI_IN);
	printf("%d ?== %d\n", record_data.a, record_data_read.a);
	assert(record_data.a == record_data_read.a);
	printf("%d ?== %d\n", b, b_read);
	assert(b == b_read);
	
	PDI_finalize();
	return 0;
}
