/*
 * SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <stdlib.h>
#include <pdi.h>


int which_event_ran = 0;

const char* CONFIG_YAML
	= "logging: trace                 \n"
	  "metadata:                      \n"
	  "data:                          \n"
	  "  test_var: double             \n"
	  "  input: int                   \n"
	  "  output: int                  \n"
	  "plugins:                       \n"
	  "  user_code:                   \n"
	  "    on_data:                   \n"
	  "      exposed:                 \n"
	  "        datf: {value: $exposed}\n"
	  "    on_event:                  \n"
	  "      exposed:                 \n"
	  "        evtf: {value: $exposed}\n";

void datf(void)
{
	void* value;
	PDI_access("value", &value, PDI_IN);
	which_event_ran += *((int*)value) + 100;
	PDI_release("value");
}

void evtf(void)
{
	void* value;
	PDI_access("value", &value, PDI_IN);
	which_event_ran += *((int*)value) + 200;
	PDI_release("value");
}

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_string(CONFIG_YAML));

	int exposed = 10;
	PDI_expose("exposed", &exposed, PDI_OUT);
	if (which_event_ran != 110) {
		fprintf(stderr, "Invalid value!\n");
		abort();
	}

	PDI_finalize();
}
