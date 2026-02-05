/*
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <paraconf.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

	int size = 0;
	PDI_expose("dummy_0", &size, PDI_NONE);

	PDI_event("trace_level");

	PDI_expose("dummy_1", &size, PDI_NONE);

	PDI_event("info_level");

	PDI_expose("dummy_2", &size, PDI_NONE);

	PDI_finalize();
}
