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

void write_grid()
{
	Grid data_write;
	alloc_grid(&data_write);
	init_grid(&data_write);
	
	int input = 0;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("grid_data", &data_write, PDI_OUT);
	
	free_grid(&data_write);
}

void check_read_grid(const Grid* data_read)
{
	Grid data;
	alloc_grid(&data);
	init_grid(&data);
	
	assert_eq_grid(data_read, &data);
	free_grid(&data);
}

void read_grid()
{
	Grid data_read;
	alloc_grid(&data_read);
	
	int input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("grid_data", &data_read, PDI_IN);
	
	check_read_grid(&data_read);
	free_grid(&data_read);
}

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	
	write_grid();
	read_grid();
	
	PDI_finalize();
	return 0;
}
