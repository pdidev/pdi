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
#include <pdi.h>
#include <stdio.h>

/*void write_subvector()
{
	// Subvector data_write;
	// alloc_subvector(&data_write);
	// init_subvector(&data_write);
	PDI_multi_expose("write", "subvector", &data_write, PDI_OUT, NULL);

	// free_subvector(&data_write);
}*/

/*int main(int argc, char* argv[])
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

	// // # As the value is shared with pdi.h, it can be reclaimed in a second time.
	// // # In the case of pdi_deactivation.h, the value is never shared, hence the reclaim operation fails.
	// PDI_share("global_size",    global_size,    PDI_OUT) == NULL;
	// PDI_reclaim("global_size");

	write_data_serialized();

	PDI_finalize();
	return 0;
}*/

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_path(argv[1]);

	PDI_init(PC_get(conf, ".pdi"));
	printf("%s", argv[1]);


	int readOrWrite = 0;
	PDI_expose("readOrWrite", &readOrWrite, PDI_OUT);

	int scalar_data = 42;
	PDI_expose("scalar_data", &scalar_data, PDI_OUT);
	int array_data[8];
	for (int i = 0; i < 8; i++) {
		array_data[i] = 42 + i;
	}
	// PDI_expose("array_data", array_data, PDI_OUT);
	PDI_multi_expose("write", "array_data", &array_data, PDI_OUT, NULL);


	readOrWrite = 1;
	PDI_expose("readOrWrite", &readOrWrite, PDI_OUT);


	int scalar_data_read;
	PDI_expose("scalar_data", &scalar_data_read, PDI_IN);
	printf("%d ?== %d\n", scalar_data, scalar_data_read);
	assert(scalar_data == scalar_data_read);

	int array_data_read[8];
	PDI_expose("array_data", array_data_read, PDI_IN);
	for (int i = 2; i < 6; i++) {
		printf("[%d] %d ?== %d\n", i, array_data[i], array_data_read[i]);
		assert(array_data[i] == array_data_read[i]);
	}


	PDI_finalize();
	return 0;
}
