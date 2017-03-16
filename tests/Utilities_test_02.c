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

#define IMX 10
#define JMX 5
#define JGH 1
#define IGH 2

int main( int argc, char *argv[] )
{

	int values[JMX][IMX], cp_values[JMX-2*JGH][IMX-2*IGH];
	double reals[JMX][IMX], cp_reals[JMX-2*JGH][IMX-2*IGH];
	int i, j;
	int ni=IMX, nj=JMX, ni_gh=IGH, nj_gh=JGH; // PDI only


	MPI_Init(&argc, &argv);
	assert(argc == 2 && "Needs 1 single arg: config file");

	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_status_t err = PDI_init(PC_get(conf,".pdi"), &world);
	int rank; MPI_Comm_rank(world, &rank);

	// Fill arrays 
	for(j=0;j<JMX;++j){
		for(i=0;i<IMX;++i){
			values[j][i]= -1;
			reals[j][i] = -1.0;
		}
	}

	for(j=0; j<JMX-2*JGH; ++j){
		for(i=0; i<IMX-2*IGH; ++i){
			cp_values[j][i]= 0;
			cp_reals[j][i] = 0.0;
			values[j+JGH][i+IGH]= i+1+10*(double)(j);
			reals[j+JGH][i+IGH] = (double)(i+1)+0.1*((i+1)%10)+10*(double)(j);
		}
	}

	PDI_expose("rank",&rank);
	// Set size for PDI
	PDI_expose("ni",&ni);
	PDI_expose("nj",&nj);
	PDI_expose("ni_ghost",&ni_gh);
	PDI_expose("nj_ghost",&nj_gh);

	// Export data => produce a buffer => buffer is exported in hdf5
	PDI_expose("reals",&reals );     // output real
	PDI_expose("values",&values );   // output integers
	
	// reimport buffer (read hdf5)
	PDI_import("cp_reals",&cp_reals);
	PDI_import("cp_values",&cp_values);


	fprintf(stderr, "Comparing arrays. -1 indicates ghost cells.",j); 
	for(j=0;j<JMX;++j){
		fprintf(stderr, "\n Values (exported) j= %d\n",j); 
		for(i=0;i<IMX;++i){
			fprintf(stderr, " %4d  |",values[j][i]); 
		}
	}
	for(j=0; j<JMX-2*JGH; ++j){
		fprintf(stderr, "\n Values (read) j= %d\n",j); 
		for(i=0; i<IMX-2*IGH; ++i){
			fprintf(stderr, " %4d  |", cp_values[j][i]); 
			assert( cp_values[j][i] == values[j+JGH][i+IGH]);
			assert( cp_reals[j][i]  == reals[j+JGH][i+IGH] ); 
		}
	}


	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
	return 0;
}
