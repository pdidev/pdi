/*******************************************************************************
 * Copyright (C) 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * 
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
// #include <pdi.h>
#include <pdi_deactivation.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_path(argv[1]);

	PDI_init(PC_get(conf, ".pdi"));
	printf("%s", argv[1]);

	long longval;

	int global_size[2];
	PC_int(PC_get(conf, ".global_size.height"), &longval);
	global_size[0] = longval;
	PC_int(PC_get(conf, ".global_size.width"), &longval);
	global_size[1] = longval;

	// # As the value is shared with pdi.h, it can be reclaimed in a second time.
	// # In the case of pdi_deactivation.h, the value is never shared, hence the reclaim operation fails.
	// PDI_share("global_size",    global_size,    PDI_OUT) == NULL;
	PDI_reclaim("global_size");

	PDI_finalize();
	return 0;
}
