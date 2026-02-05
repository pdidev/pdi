/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
	Record_data record;
	PDI_expose("record_data", &record, PDI_IN);
	printf("Scalar: %d\n", record.scalar_data);
	assert(record.scalar_data == 4);
	for (int i = 0; i < 4; i++) {
		printf("Array[%d] = %d\n", i, record.array_data[i]);
		assert(record.array_data[i] == i + 2);
	}
	PDI_finalize();
}
