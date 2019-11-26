/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

struct Record {
	int id;
	int value[16];
};

int main( int argc, char* argv[] )
{
	struct Record rec[4];
	// init data
	for (int i = 0; i < 4; i++) {
		rec[i].id = i;
		for (int j = 0; j < 16; j++) {
			rec[i].value[j] = j + i*16;
		}
	}
	PDI_init(PC_parse_path(argv[1]));
	int input = 0;
	PDI_expose("input", &input, PDI_OUT);
	
	// write record to file
	PDI_expose("array_of_record", rec, PDI_OUT);
	
	// load record from file
	input = 1;
	PDI_expose("input", &input, PDI_OUT);
	struct Record rec_read[2];
	PDI_expose("array_of_record", rec_read, PDI_IN);
	
	// check values
	for (int i = 1; i < 3; i++) {
		assert(rec[i].id == i);
		for (int j = 0; j < 16; j++) {
			assert(rec[i].value[j] == j + i*16);
		}
	}
	PDI_finalize();
}
