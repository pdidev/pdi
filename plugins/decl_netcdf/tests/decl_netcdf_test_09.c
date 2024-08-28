/*******************************************************************************
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

typedef struct Inner_record_s {
	int int_scalar;
	double double_array[32];
} Inner_record_t;

typedef struct Outer_record_s {
	char char_scalar;
	int int_array[32];
	Inner_record_t inner_record;
} Outer_record_t;

// Tests simple write and read of scalar and array depending on event
int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

	// init data
	Outer_record_t record_data;
	record_data.char_scalar = 42;
	for (int i = 0; i < 32; i++) {
		record_data.int_array[i] = i;
	}
	record_data.inner_record.int_scalar = 84;
	for (int i = 0; i < 32; i++) {
		record_data.inner_record.double_array[i] = i * 1.23;
	}

	// write data
	PDI_multi_expose("write", "outer_record", &record_data, PDI_OUT, NULL);

	// zero data
	record_data.char_scalar = 0;
	for (int i = 0; i < 32; i++) {
		record_data.int_array[i] = 0;
	}
	record_data.inner_record.int_scalar = 0;
	for (int i = 0; i < 32; i++) {
		record_data.inner_record.double_array[i] = 0.0;
	}

	// read data
	PDI_multi_expose("read", "outer_record", &record_data, PDI_IN, NULL);

	// verify
	int status = 0;
	if (record_data.char_scalar != 42) {
		printf("record_data.char_scalar = %d != %d\n", record_data.char_scalar, 42);
		status = 1;
	}
	for (int i = 0; i < 32; i++) {
		if (record_data.int_array[i] != i) {
			printf("record_data.int_array[%d] = %d != %d\n", i, record_data.char_scalar, i);
			status = 1;
		}
	}
	if (record_data.inner_record.int_scalar != 84) {
		printf("record_data.inner_record.int_scalar = %d != %d\n", record_data.inner_record.int_scalar, 84);
		status = 1;
	}
	for (int i = 0; i < 32; i++) {
		if (record_data.inner_record.double_array[i] != i * 1.23) {
			printf("record_data.inner_record.double_array[%d] = %f != %f\n", i, record_data.inner_record.double_array[i], i * 1.23);
			status = 1;
		}
	}

	PDI_finalize();
	return status;
}
