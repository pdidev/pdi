/*
 * SPDX-FileCopyrightText: 2021-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <pdi.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path("hello_data.yml"));
	int my_world = 42;
	PDI_share("world", &my_world, PDI_OUT);
	//variable my_world is shared with PDI

	PDI_reclaim("world");
	//variable my_world is no longer shared with PDI

	PDI_finalize();
	return 0;
}
