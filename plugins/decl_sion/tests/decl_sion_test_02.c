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

const int IMX = 10;
const int JMX = 5;

const char* CONFIG_YAML =
"metadata:                  \n"
"  input: int               \n"
"  ni: int                  \n"
"  nj: int                  \n"
"data:                      \n"
"  reals:                   \n"
"    type: double           \n"
"    sizes: [$nj , $ni]     \n"
"  values:                  \n"
"    type: int              \n"
"    sizes: [$nj , $ni]     \n"
"                           \n"
"plugins:                   \n"
"  decl_sion:               \n"
"    outputs:               \n"
"      - variable: reals    \n"
"        file: reals_C.sion \n"
"        select: $input = 0 \n"
"      - variable: values   \n"
"        file: values_C.sion\n"
"        select: $input = 0 \n"
"      - variable: ni       \n"
"        file: ni_C.sion    \n"
"        select: $input = 0 \n"
"      - variable: nj       \n"
"        file: nj_C.sion    \n"
"        select: $input = 0 \n"
"    inputs:                \n"
"      - variable: reals    \n"
"        file: reals_C.sion \n"
"        select: $input = 1 \n"
"      - variable: values   \n"
"        file: values_C.sion\n"
"        select: $input = 1 \n"
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
	
	int input=0;
	PDI_expose("input",&input, PDI_OUT);
	// Set size for PDI
	PDI_expose("ni", &IMX, PDI_OUT);
	PDI_expose("nj", &JMX, PDI_OUT);
	
	// Test that export/exchange works
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("reals", &reals, PDI_OUT);      // output real
	PDI_expose("values", &values, PDI_INOUT);  // output integers
	
	input=1;
	// Import should also work
	PDI_expose("input", &input, PDI_OUT); // update metadata => decl_sion now import only
	PDI_expose("reals", &cp_reals, PDI_IN);      // input real
	PDI_expose("values", &cp_values, PDI_INOUT); // input integers
	
	// So the data should be the same
	fprintf(stderr,"Data exported | Data imported\n");
	for (int j=0; j<JMX; ++j) {
		for (int i=0; i<IMX; ++i) {
			fprintf(stderr,"%10d     %4d\n", values[j][i], cp_values[j][i]);
			fprintf(stderr,"%10.2f     %2.2f\n", reals[j][i], cp_reals[j][i]);
			if ( values[j][i] != cp_values[j][i] ) abort();
			if ( reals[j][i]  != cp_reals[j][i]  ) abort();
		}
	}
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
}
