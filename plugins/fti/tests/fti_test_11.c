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

#include <assert.h>
#include <mpi.h>
#include <pdi.h>
#include <fti.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_path(argv[1]);
	PDI_init(conf);
	MPI_Comm* FTI_Comm;
	PDI_access("FTI_COMM_WORLD", (void**)&FTI_Comm, PDI_IN);
	int rank;
	MPI_Comm_rank(*FTI_Comm, &rank);
	PDI_release("FTI_COMM_WORLD");
	PDI_expose("rank", &rank, PDI_OUT);
	
	char file[64];
	sprintf(file, "./dummy_file%d", rank);
	FILE* fstream = fopen(file, "wb+");
	fclose(fstream);
	assert(access(file, F_OK) != -1);
	
	PDI_event("send_one_file");
	int file_status = 0;
	PDI_expose("file1_status", &file_status, PDI_IN);
	
	assert(file_status == FTI_SI_SCES);
	assert(access(file, F_OK) == -1);
	
	sprintf(file, "./dummy_file%d_sent", rank);
	assert(access(file, F_OK) != -1);
	
	sprintf(file, "./first_file%d", rank);
	fstream = fopen(file, "wb+");
	fclose(fstream);
	assert(access(file, F_OK) != -1);
	
	char second_file[64];
	sprintf(second_file, "./second_file%d", rank);
	fstream = fopen(second_file, "wb+");
	fclose(fstream);
	assert(access(second_file, F_OK) != -1);
	
	PDI_event("send_two_files");
	PDI_expose("file2_status", &file_status, PDI_IN);
	assert(file_status == FTI_SI_SCES);
	PDI_expose("file3_status", &file_status, PDI_IN);
	assert(file_status == FTI_SI_SCES);
	
	assert(access(file, F_OK) == -1);
	assert(access(second_file, F_OK) == -1);
	
	sprintf(file, "./first_file%d_sent", rank);
	sprintf(second_file, "./second_file%d_sent", rank);
	
	assert(access(file, F_OK) != -1);
	assert(access(second_file, F_OK) != -1);
	
	PDI_finalize();
	MPI_Finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}
