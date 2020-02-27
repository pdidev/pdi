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
#include <paraconf.h>
#include <pdi.h>

const char* CONFIG_YAML =
    "logging: trace          \n"
    "data:                   \n"
    "  invalid:              \n"
    "    type: array         \n"
    "    subtype: double\n"
    "    size: $meta2        \n"
    "plugins:                \n"
    ;

int main( int argc, char* argv[] )
{
	PDI_init(PC_parse_string(CONFIG_YAML));
	
	PDI_errhandler(PDI_NULL_HANDLER);
	double invalid;
	PDI_status_t err = PDI_expose("invalid", &invalid, PDI_INOUT);
	if ( err != PDI_ERR_VALUE ) abort();
	if ( strcmp(PDI_errmsg(), "while referencing `meta2': Cannot access a non shared value: `meta2'") ) abort();
	
	PDI_finalize();
}
