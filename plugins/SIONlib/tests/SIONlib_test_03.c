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

int main( int argc, char *argv[] )
{
	int values[JMX][IMX], cp_values[JMX][IMX];
	double reals[JMX][IMX], cp_reals[JMX][IMX];
	int i, j;
	int ni=IMX,nj=JMX; // PDI only

	MPI_Init(&argc, &argv);
	assert(argc == 2 && "Needs 1 single arg: config file");

	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_status_t err = PDI_init(PC_get(conf,".pdi"), &world);
	int rank; MPI_Comm_rank(world, &rank);

	// Fill arrays
	for(j=0;j<JMX;++j){
		for(i=0;i<IMX;++i){
			values[j][i]= i;
			reals[j][i] = (double)(i)+0.1*(i%10);
			cp_values[j][i]= -1;
			cp_reals[j][i] = -1.0;
		}
	}

	// Set size for PDI
	PDI_expose("ni",&ni);
	PDI_expose("nj",&nj);

	// Test that expose works
        PDI_transaction_begin("write_data");
	PDI_expose("reals",&reals );     // output real
	PDI_expose("values",&values ); // output integers
        PDI_transaction_end();

	// Exchange should also work
        PDI_transaction_begin("read_data");
	PDI_exchange("reals" ,&cp_reals);     // input real
	PDI_exchange("values" ,&cp_values); // input integers
        PDI_transaction_end();

	// So the data should be the same
	fprintf(stderr,"Data exported | Data imported\n");
	for(j=0;j<JMX;++j){
		for(i=0;i<IMX;++i){
			fprintf(stderr,"%10d     %4d\n", values[j][i], cp_values[j][i]);
			fprintf(stderr,"%10.2f     %2.2f\n", reals[j][i], cp_reals[j][i]);
			assert( values[j][i] ==  cp_values[j][i]);
			assert( reals[j][i]  ==  cp_reals[j][i] );
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
	return 0;
}
