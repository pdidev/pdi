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

#include <mpi.h>
#include <pdi.h>

const char PDI_CONFIG[] = "\
logging: trace\n\
data:\n\
  fti_status: int\n\
  test: int\n\
  array: {type: array, size: 10, subtype: int}\n\
  array2D: {type: array, size: [3, 3], subtype: int}\n\
  test_size: int64\n\
  array_size: int64\n\
  array2D_size: int64\n\
plugins:\n\
  mpi:\n\
  fti:\n\
    config_file: fti_config_03.ini\n\
    status: fti_status\n\
    dataset:\n\
      0: { name: test, size: test_size }\n\
      1: { name: array, size: array_size }\n\
      2: { name: array2D, size: array2D_size }\n\
    recover_on: recover\n\
    checkpoint:\n\
      L1_on: ckpt\n\
";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(PDI_CONFIG);
	PDI_init(conf);
	
	int test = 10;
	int array[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 123456789 };
	int array2D[3][3] = { 11, 12, 13, 21, 22, 23, 31, 32, 33 };
	long test_size = -1;
	long array_size = -2;
	long array2D_size = -3;
	
	
	PDI_multi_expose("ckpt",
	    "test", &test, PDI_OUT,
	    "array", array, PDI_OUT,
	    "array2D", array2D, PDI_OUT,
	    NULL);
	    
	int status;
	PDI_expose("fti_status", &status, PDI_IN);
	if ( status != 0 ) {
		fprintf(stderr, "Error, expected status == 0, got status=%d\n", status);
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	
	PDI_expose("test_size", &test_size, PDI_IN);
	PDI_expose("array_size", &array_size, PDI_IN);
	PDI_expose("array2D_size", &array2D_size, PDI_IN);
	
	if ( test_size != sizeof(test) ) {
		fprintf(stderr, "Before recover, error, expected test_size == sizeof(test), got test_size=%d, sizeof(test)=%d\n", test_size, sizeof(test));
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	if ( array_size != sizeof(array) ) {
		fprintf(stderr, "Before recover, error, expected array_size == sizeof(array), got array_size=%d, sizeof(array)=%d\n", array_size, sizeof(array));
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	if ( array2D_size != sizeof(array2D) ) {
		fprintf(stderr, "Before recover, error, expected array2D_size == sizeof(array2D), got array2D_size=%d, sizeof(array2D)=%d\n", array2D_size, sizeof(array2D));
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	
	test_size = array_size = array2D_size = 0;
	test = -10;
	test_size = -10;
	array_size = -20;
	array2D_size = -30;
	
	PDI_finalize();
	
	//recovery
	PDI_init(conf);
	
	PDI_expose("test_size", &test_size, PDI_IN);
	PDI_expose("array_size", &array_size, PDI_IN);
	PDI_expose("array2D_size", &array2D_size, PDI_IN);
	
	if ( test_size != sizeof(test) ) {
		fprintf(stderr, "After recover, error, expected test_size == sizeof(test), got test_size=%d, sizeof(test)=%d\n", test_size, sizeof(test));
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	if ( array_size != sizeof(array) ) {
		fprintf(stderr, "After recover, error, expected array_size == sizeof(array), got array_size=%d, sizeof(array)=%d\n", array_size, sizeof(array));
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	if ( array2D_size != sizeof(array2D) ) {
		fprintf(stderr, "After recover, error, expected array2D_size == sizeof(array2D), got array2D_size=%d, sizeof(array2D)=%d\n", array2D_size, sizeof(array2D));
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
	
	PDI_finalize();
	MPI_Finalize();
	
	return 0;
}
