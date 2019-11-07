/*******************************************************************************
 * Copyright (C) 2019 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

struct XYZ {
	int x;
	int y;
	int z;
};

struct Record {
	int id;
	struct XYZ value[4]; // 2 x 2
};

int main( int argc, char* argv[] )
{
	struct Record outer_record;

	// init data
	outer_record.id = 24;
	for (int i = 0; i < 4; i++) {
		outer_record.value[i].x = -1*i;
		outer_record.value[i].y = 2*i;
		outer_record.value[i].z = 3*i;
	}

	PDI_init(PC_parse_path(argv[1]));
	int input = 0;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("outer_record", &outer_record, PDI_OUT);

	// reset record
	outer_record.id = 0;
	for (int i = 0; i < 4; i++) {
		outer_record.value[i].x = 0;
		outer_record.value[i].y = 0;
		outer_record.value[i].z = 0;
	}

	// load record from file
	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("outer_record", &outer_record, PDI_IN);

	// check values
	printf("%d != %d\n", outer_record.id, 24);
	assert(outer_record.id == 24);
	for (int i = 0; i < 4; i++) {
		assert(outer_record.value[i].x == -1*i);
		assert(outer_record.value[i].y == 2*i);
		assert(outer_record.value[i].z == 3*i);
	}
	PDI_finalize();
}
