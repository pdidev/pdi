/*
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <pdi.h>

typedef struct Record {
	int int_scalar;
	double double_array[32];
} Record_t;

// Tests simple write and read of scalar and array depending on event
int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

	// init data
	Record_t record_data;
	record_data.int_scalar = 42;
	for (int i = 0; i < 32; i++) {
		record_data.double_array[i] = 42.4242;
	}

	// write data
	PDI_multi_expose("write", "record", &record_data, PDI_OUT, NULL);

	// zero data
	record_data.int_scalar = 0;
	for (int i = 0; i < 32; i++) {
		record_data.double_array[i] = 0.0;
	}


	// read data
	PDI_multi_expose("read", "record", &record_data, PDI_IN, NULL);

	// verify
	int status = 0;
	if (record_data.int_scalar != 42) {
		printf("record_data.int_scalar = %d != %d\n", record_data.int_scalar, 42);
		status = 1;
	}
	for (int i = 0; i < 32; i++) {
		if (record_data.double_array[i] != 42.4242) {
			printf("record_data.double_array = %d != %d\n", record_data.double_array[i], 42.4242);
			status = 1;
		}
	}



	PDI_finalize();
	return status;
}
