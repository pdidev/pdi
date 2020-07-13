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

void write_vector()
{
	Vector data_write;
	alloc_vector(&data_write);
	init_vector(&data_write);
	
	int input = 0;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("vector_data", &data_write, PDI_OUT);
	
	free_vector(&data_write);
}

void check_read_vector(const Vector* data_read)
{
	Vector data;
	alloc_vector(&data);
	init_vector(&data);
	
	assert_eq_vector(data_read, &data);
	free_vector(&data);
}

void read_vector()
{
	Vector data_read;
	alloc_vector(&data_read);
	
	int input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("vector_data", &data_read, PDI_IN);
	
	check_read_vector(&data_read);
	free_vector(&data_read);
}

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	
	write_vector();
	read_vector();
	
	PDI_finalize();
	return 0;
}
