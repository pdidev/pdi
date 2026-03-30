/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

const char CONF_YAML[] =
    "metadata:\n"
    "  ii: int\n"
    "data:\n"
    "  cp_status: int\n"
    "  cp_counter: int\n"
    "  a: int\n"
    "plugins:\n"
    "  veloc:\n"
    "    failure: 0\n"
    "    config_file: veloc_config.cfg\n"
    "    status: cp_status\n"
    "    counter: cp_counter\n"
    "    checkpoint_label: test_01\n"
    "    iteration: ii\n"
    "    protect_data: [ii, a]\n"   
    "    checkpoint_on: ckp\n";


int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(CONF_YAML);
	PDI_init(conf);
	
	int cp_status;
	int cp_counter; 
	int ii = 0; 
	int a = 30;
	
	PDI_multi_expose("init", "ii", &ii, PDI_INOUT,
			"a", &a, PDI_INOUT, NULL);
	PDI_expose("cp_status", &cp_status, PDI_IN);

	assert(cp_status== 1); // recovery not needed 

	ii = 20;
	a = 50;

	PDI_multi_expose("ckp", "ii", &ii, PDI_INOUT,
			"a", &a, PDI_INOUT, NULL);
	PDI_expose("cp_counter", &cp_counter, PDI_IN);
	printf("counter is %d/n", cp_counter);
	assert(cp_counter == 1);

    printf("TEST 01_1 PASSED ");
	
	PDI_finalize();
	MPI_Finalize();
	
	return 0;
}
