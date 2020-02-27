/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include <assert.h>
#include <stdlib.h>
#include <pdi.h>


int which_event_ran = 0;

const char* CONFIG_YAML =
    "logging: trace                 \n"
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
    "        evtf: {value: $exposed}\n"
    ;

void datf(void)
{
	void* value; PDI_access("value", &value, PDI_IN);
	which_event_ran += *((int*)value) + 100;
	PDI_release("value");
}

void evtf(void)
{
	void* value; PDI_access("value", &value, PDI_IN);
	which_event_ran += *((int*)value) + 200;
	PDI_release("value");
}

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_string(CONFIG_YAML));
	
	int exposed = 10;
	PDI_expose("exposed", &exposed, PDI_OUT);
	if ( which_event_ran != 110 ) {
		fprintf(stderr, "Invalid value!\n");
		abort();
	}
	
	PDI_finalize();
}
