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

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	
	Vector vector;
	alloc_vector(&vector);
	init_vector(&vector);
	
	print_vector(&vector);
	
	PDI_share("vector_data", &vector, PDI_OUT);
	
	VectorSerialized* vector_serialized;
	PDI_access("vector_data_serialized", (void**)&vector_serialized, PDI_IN);
	
	assert_eq_vector_serialized(&vector, vector_serialized);
	
	PDI_release("vector_data_serialized");
	
	PDI_reclaim("vector_data");
	
	free_vector(&vector);
	
	PDI_finalize();
	return 0;
}