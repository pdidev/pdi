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
#include <regex.h>
#include <unistd.h>

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_path(argv[1]);
	PDI_init(conf);
	
	char stage_dir[256] = {0};
	PDI_expose("fti_stage_dir", stage_dir, PDI_IN);
	
	regex_t dir_regex;
	assert(!regcomp(&dir_regex,"^\\./fti_ckpt/.*/stage$", 0) && "Can't compile regex");
	
	assert(regexec(&dir_regex, stage_dir, 0, NULL, 0) == 0 && "Stage dir doesn't match regex");
	
	PDI_finalize();
	MPI_Finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}
