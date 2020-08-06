/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi.h>
#include <assert.h>

#include "serialize_test.h"

void write_subvector()
{
	Subvector data_write;
	alloc_subvector(&data_write);
	init_subvector(&data_write);
	PDI_multi_expose("write",
	    "subvector", &data_write, PDI_OUT,
	    NULL);
	    
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
