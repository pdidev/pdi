/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

const int IMX = 1;
const int JMX = 5;

const char* CONFIG_YAML =
    "metadata:                            \n"
    "  ni: int                            \n"
    "  nj: int                            \n"
    "data:                                \n"
    "  reals:                             \n"
    "    type: double                     \n"
    "    sizes: [$nj , $ni]               \n"
    "  values:                            \n"
    "    type: int                        \n"
    "    sizes: [$nj , $ni]               \n"
    "                                     \n"
    "plugins:                             \n"
    "  decl_sion:                         \n"
    "    inputs:                          \n"
    "      - event: read_data             \n"
    "        file: test_03_C.sion         \n"
    "        vars: [values, reals]        \n"
    "    outputs:                         \n"
    "      - event: write_data            \n"
    "        file: test_03_C.sion         \n"
    "        vars: [ni, nj, reals, values]\n"
    ;

int main(int argc, char* argv[])
{
	int values[JMX][IMX], cp_values[JMX][IMX];
	double reals[JMX][IMX], cp_reals[JMX][IMX];
	
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_init(conf, &world);
	
	// Fill arrays
	for (int j=0; j<JMX; ++j) {
		for (int i=0; i<IMX; ++i) {
			values[j][i]= i;
			reals[j][i] = (double)(i)+0.1*(i%10);
			cp_values[j][i]= -1;
			cp_reals[j][i] = -1.0;
		}
	}
	
	// Set size for PDI
	PDI_expose("ni", &IMX, PDI_OUT);
	PDI_expose("nj", &JMX, PDI_OUT);
	
	// Test that expose works
	PDI_multi_expose("write_data",
	    "reals",&reals, PDI_OUT,   // output real
	    "values",&values, PDI_OUT, // output integers
	    NULL);
	    
	// Exchange should also work
	PDI_multi_expose("read_data",
	    "reals",&cp_reals, PDI_INOUT, // input real
	    "values",&cp_values, PDI_IN,  // input integers
	    NULL);
	    
	// So the data should be the same
	for (int j=0; j<JMX; ++j) {
		for (int i=0; i<IMX; ++i) {
			if ( values[j][i] != cp_values[j][i]) abort();
			if ( reals[j][i]  != cp_reals[j][i]) abort();
		}
	}
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
}
