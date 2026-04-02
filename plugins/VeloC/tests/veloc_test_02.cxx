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
#include <iostream>

const char CONF_YAML[] =
    "metadata:\n"
    "  ii: int\n"
    "  arr_size: int\n"
    "data:\n"
    "  cp_status: int\n"
    "  cp_counter: int\n"
    "  arr:  {type: array, size: '$arr_size', subtype: double}\n"
    "plugins:\n"
    "  veloc:\n"
    "    failure: 0\n"
    "    config_file: veloc_config.cfg\n"
    "    counter: cp_counter\n"
	"    status: cp_status\n"
    "    checkpoint_label: test_02\n"
    "    iteration: ii\n"
    "    protect_data: [ii, arr]\n"   
    "    synchronize_on: sync\n"
	"    when: '$ii % 2 = 0 '\n";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(CONF_YAML);
	PDI_init(conf);
	
	int cp_counter; 
	int cp_status; 

	int ii = 0; 
	int arr_size = 20; 
	PDI_expose("arr_size", &arr_size, PDI_OUT);
	double * arr = new double[arr_size];
	for (int i = 0; i < arr_size; i++) arr[i] = 0.0;

	PDI_expose("cp_status", &cp_status, PDI_IN);
	
	for (; ii<10; ++ ii) {

		arr[ii] = ii * 2; 
		PDI_multi_expose("sync",
                "ii",        &ii,       PDI_INOUT,
                "arr", arr, PDI_INOUT,
                NULL);
		
		if((cp_status) && (ii == 5)){
			break; 
		}
    };

	PDI_expose("cp_counter", &cp_counter, PDI_IN);
	
	if(cp_status){ // no recovery needed
		if(cp_counter!=3){
			std::cerr << "TEST_02 FAILED: counter value " << cp_counter
                  << " does not match expected value " << 3 << std::endl;
			exit(1);
		}
	}
	else if (!cp_status){ // recovery needed 
		if(cp_counter!=2){
		std::cerr << "TEST_02 FAILED: counter value " << cp_counter
                  << " does not match expected value " << 2 << std::endl;
		exit(1);
		}
	}

    printf("TEST 02 PASSED ");
	
	PDI_finalize();
	MPI_Finalize();
	
	return 0;
}
