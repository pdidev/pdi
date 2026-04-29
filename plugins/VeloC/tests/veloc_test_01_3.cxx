/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <iostream>
#include <pdi.h>

const char CONF_YAML[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  cp_status: int\n"
	  "  cp_counter: int\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 1\n"
	  "    config_file: veloc_config.cfg\n"
	  "    status: cp_status\n"
	  "    counter: cp_counter\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    managed_checkpointing:\n"
	  "      protect_data: [ii, var]\n"
	  "      recover_on: recover\n"
	  "      checkpoint_nr : 0\n";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(CONF_YAML);
	PDI_init(conf);

	int cp_status;
	int rec_ii = -1;
	int rec_var = 0;

	PDI_expose("cp_status", &cp_status, PDI_IN);

	if (cp_status != 0) {
		std::cerr << "TEST_01_3 FAILED: status value " << cp_status << " does not match expected value " << 0 << std::endl;
		exit(1);
	}

	PDI_multi_expose("recover", "ii", &rec_ii, PDI_INOUT, "var", &rec_var, PDI_INOUT, NULL);

	PDI_expose("cp_status", &cp_status, PDI_IN);

	if (cp_status != 1) {
		std::cerr << "TEST_01_3 FAILED: status value " << cp_status << " does not match expected value " << 1 << std::endl;
		exit(1);
	}

	if (rec_ii != 0) {
		std::cerr << "TEST_01_3 FAILED: recovered iter value " << rec_ii << " does not match expected value " << 0 << std::endl;
		exit(1);
	}

	if (rec_var != 51) {
		std::cerr << "TEST_01_3 FAILED: recovered var value " << rec_var << " does not match expected value " << 51 << std::endl;
		exit(1);
	}

	std::cout << "TEST 01_3 PASSED " << std::endl;

	PDI_finalize();
	MPI_Finalize();

	return 0;
}