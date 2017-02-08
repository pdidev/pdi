/*******************************************************************************
 * Copyright (c) 2015, Corentin Roussel - CEA (corentin.roussel@cea.fr)
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
#include <unistd.h>
#include <mpi.h>
#include <pdi.h>


int main( int argc, char *argv[])
{
	int value[5]={0,1,2,3,4};
	int i ,err;
	double test_var=0;
	const char *filename="empty_file.txt";
	MPI_Init(&argc, &argv);
	assert(argc == 2 && "Needs 1 single arg: config file");
	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm world = MPI_COMM_WORLD;
	err = PDI_init(PC_get(conf,".pdi"), &world);

	// initialize metadata
	PDI_expose("meta0",&value[0]);
	PDI_expose("meta1",&value[1]);
	PDI_expose("meta2",&value[2]);
	PDI_expose("meta3",&value[3]);
	PDI_expose("nth_meta",&value[2]);

	// ------- Checking that "file_exists" action works
	remove(filename); // remove file if it exists
	PDI_event("init"); // check file exist 'empty_file.txt'
	test_var=0;
	err=PDI_import("test_var",&test_var); // utilities plug-in provides the value
	assert( !test_var && "Import should fail. File has been removed"); 

	FILE *fp = fopen(filename, "ab+");
	fflush(fp);
	fclose(fp);
	PDI_event("init"); // check file exist 'empty_file.txt'
	test_var=0;
	err=PDI_import("test_var",&test_var); // utilities plug-in provides the value
	assert( (int)(test_var) !=0 && "Import should succeed. File has been created");


	// ------- Checking that "event2data" action works
	PDI_event("casual_event"); // compute meta0+meta1 = 0+1 -> meta2
	PDI_event("casual_event2"); // meta0-meta2 = -1

	PDI_import("meta2",&i); // meta2=meta0+meta1
	// assert(i==1 && "should be 1");
	PDI_import("meta3",&i);
	// assert(i==-1 && "should be -1 ");

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
	return 0;
}
