/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <pdi.h>

#include "test.h"

void write_grid()
{
	Grid data_write;
	alloc_grid(&data_write);
	init_grid(&data_write);

	int input = 0;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("grid_data", &data_write, PDI_OUT);

	free_grid(&data_write);
}

void check_read_grid(const Grid* data_read)
{
	Grid data;
	alloc_grid(&data);
	init_grid(&data);

	assert_eq_grid(data_read, &data);
	free_grid(&data);
}

void read_grid()
{
	Grid data_read;
	alloc_grid(&data_read);

	int input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("grid_data", &data_read, PDI_IN);

	check_read_grid(&data_read);
	free_grid(&data_read);
}

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

	write_grid();
	read_grid();

	PDI_finalize();
	return 0;
}
