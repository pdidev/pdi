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

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	
	// allocate memory
	double* pointer = malloc(sizeof(double));
	int* array = malloc(8 * sizeof(int));
	int** pointer_to_array = &array;
	int** pointer_array = malloc(8 * sizeof(int*));
	printf("pointer_to_array: %p\n", pointer_to_array);
	for (int i = 0; i < 8; i++) {
		pointer_array[i] = malloc(sizeof(int));
		printf("pointer_to_array[%d]: %p\n", i, &array[i]);
	}
	
	// initialize data
	*pointer = 1.234;
	for (int i = 0; i < 8; i++) {
		array[i] = 42 + i;
	}
	for (int i = 0; i < 8; i++) {
		*(pointer_array[i]) = 42 + i;
	}
	
	// share
	PDI_share("pointer", &pointer, PDI_OUT);
	PDI_share("pointer_to_array", pointer_to_array, PDI_OUT);
	PDI_share("pointer_array", pointer_array, PDI_OUT);
	
	double* pointer_serialized;
	int* pointer_to_array_serialized;
	int* pointer_array_serialized;
	PDI_access("pointer_serialized", (void**)&pointer_serialized, PDI_IN);
	PDI_access("pointer_to_array_serialized", (void**)&pointer_to_array_serialized, PDI_IN);
	PDI_access("pointer_array_serialized", (void**)&pointer_array_serialized, PDI_IN);
	
	printf("%f ?== %f\n", *pointer, *pointer_serialized);
	assert(*pointer == *pointer_serialized);
	for (int i = 0; i < 8; i++) {
		printf("[%d] %d ?== %d\n", i, array[i], pointer_to_array_serialized[i]);
		assert(array[i] == pointer_to_array_serialized[i]);
	}
	for (int i = 0; i < 8; i++) {
		printf("[%d] %d ?== %d\n", i, *(pointer_array[i]), pointer_array_serialized[i]);
		assert(*(pointer_array[i]) == pointer_array_serialized[i]);
	}
	
	PDI_release("pointer_serialized");
	PDI_release("pointer_to_array_serialized");
	PDI_release("pointer_array_serialized");
	
	PDI_reclaim("pointer");
	PDI_reclaim("pointer_to_array");
	PDI_reclaim("pointer_array");
	
	free(pointer);
	free(array);
	for (int i = 0; i < 8; i++) {
		free(pointer_array[i]);
	}
	free(pointer_array);
	
	PDI_finalize();
	return 0;
}
