/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
