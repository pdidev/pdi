/*
 * SPDX-FileCopyrightText: 2021-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <pdi.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path("hello_event.yml"));

	PDI_event("Hello World Event");

	PDI_finalize();
	return 0;
}
