/*
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>

#include <paraconf.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

	int size = 4;
	PDI_expose("array_size", &size, PDI_OUT);

	int* int_array;
	PDI_access("array_metadata", (void**)&int_array, PDI_IN);
	for (int i = 0; i < 4; i++) {
		assert(int_array[i] == i + 1);
	}
	PDI_release("array_metadata");

	PDI_finalize();
}
