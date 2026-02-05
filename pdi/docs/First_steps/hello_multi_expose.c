/*
 * SPDX-FileCopyrightText: 2021-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <pdi.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path("hello_multi_expose.yml"));
	int x = 0;
	float y = 0;
	char* z = "RGB = Really Gawky Biscuit";

	PDI_multi_expose("event_between", "my_int", &x, PDI_OUT, "my_float", &y, PDI_OUT, "my_string", &z, PDI_OUT, NULL);

	PDI_finalize();
	return 0;
}
