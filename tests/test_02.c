/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <pdi.h>

#include "test.h"

void write_subvector()
{
	Subvector data_write;
	alloc_subvector(&data_write);
	init_subvector(&data_write);
	PDI_multi_expose("write", "subvector", &data_write, PDI_OUT, NULL);

	free_subvector(&data_write);
}

void check_read_subvector(const Subvector* data_read)
{
	Subvector data;
	alloc_subvector(&data);
	init_subvector(&data);

	assert_eq_subvector(data_read, &data);
	free_subvector(&data);
}

void read_subvector()
{
	Subvector data_read;
	alloc_subvector(&data_read);

	PDI_share("subvector", &data_read, PDI_IN);
	PDI_event("read");
	PDI_reclaim("subvector");

	check_read_subvector(&data_read);
	free_subvector(&data_read);
}

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

	write_subvector();
	read_subvector();

	PDI_finalize();
	return 0;
}
