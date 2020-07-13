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

struct Record {
	int scalar_member;
	double array_member[8];
	int* pointer_member;
} typedef Record;

struct Record_serialized {
	int scalar_member;
	double array_member[4];
	int pointer_member;
} typedef Record_serialized;

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	
	// initialize data
	Record record;
	record.scalar_member = 42;
	for (int i = 0; i < 8; i++) {
		record.array_member[i] = 42.123 + i;
	}
	int pointed_scalar = 50;
	record.pointer_member = &pointed_scalar;
	
	// share
	PDI_share("record", &record, PDI_OUT);
	
	Record_serialized* record_serialized;
	PDI_access("record_serialized", (void**)&record_serialized, PDI_IN);
	
	printf("%d ?== %d\n", record.scalar_member, record_serialized->scalar_member);
	assert(record.scalar_member == record_serialized->scalar_member);
	for (int i = 0; i < 4; i++) {
		printf("[%d] %f ?== %f\n", i, record.array_member[i + 2], record_serialized->array_member[i]);
		assert(record.array_member[i + 2] == record_serialized->array_member[i]);
	}
	printf("%d ?== %d\n", *record.pointer_member, record_serialized->pointer_member);
	assert(*record.pointer_member == record_serialized->pointer_member);
	
	PDI_release("record_serialized");
	PDI_reclaim("record");
	
	PDI_finalize();
	return 0;
}
