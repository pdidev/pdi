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
#include <assert.h>
#include <math.h>
#include <paraconf.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "pdi.h"


void create_coordinate_of_vertices( int dsize[2], int dstart[2], int local_size[2], int pcoord[2], double coords_x[dsize[0]+1][dsize[1]+1], double coords_y[dsize[0]+1][dsize[1]+1]){

  // catalyst variables
  int cells_ghost=1;

  size_t number_of_points[2];
  number_of_points[0] = dsize[0]+1;
  number_of_points[1] = dsize[1]+1;
  size_t total_number_of_points =  number_of_points[0]*number_of_points[1]; 

  // the first axis correspond to the y-coordinate.
  for(int ix=0; ix<number_of_points[0]; ix++) {
    for(int iy=0; iy<number_of_points[1]; iy++) {
      coords_y[ix][iy] = 1.0*(ix-dstart[0]) + pcoord[0]*(local_size[0]);
      coords_x[ix][iy] = 1.0*(iy-dstart[1]) + pcoord[1]*(local_size[1]);
    } 
  }
}

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	if (argc != 2) {
		fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
		exit(1);
	}

	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm main_comm = MPI_COMM_WORLD;

	PDI_init(PC_get(conf, ".pdi"));
	PDI_expose("mpi_comm", &main_comm, PDI_INOUT);

	int psize_1d;
	MPI_Comm_size(main_comm, &psize_1d);
	int pcoord_1d;
	MPI_Comm_rank(main_comm, &pcoord_1d);

	PDI_expose("mpi_rank", &pcoord_1d, PDI_OUT);
	PDI_expose("mpi_size", &psize_1d, PDI_OUT);
	PDI_event("init");

	long longval;

	int dsize[2];
	PC_int(PC_get(conf, ".globalsize[0]"), &longval);
	dsize[0] = longval;
	PC_int(PC_get(conf, ".globalsize[1]"), &longval);
	dsize[1] = longval;
		
	int psize[2];
	PC_int(PC_get(conf, ".parallelism.height"), &longval);
	psize[0] = longval;
	PC_int(PC_get(conf, ".parallelism.width"), &longval);
	psize[1] = longval;

	// check on distribution of MPI process
	assert(dsize[0] % psize[0] == 0);
	assert(dsize[1] % psize[1] == 0);
	assert(psize[1] * psize[0] == psize_1d);

	int cart_period[2] = {0, 0};
	MPI_Comm cart_com;
	MPI_Cart_create(main_comm, 2, psize, cart_period, 1, &cart_com);
	int pcoord[2];
	MPI_Cart_coords(cart_com, pcoord_1d, 2, pcoord);

	int dstart[2];
	int dend[2];

	int ghost_height[2]; // number of ghost in height direction
	ghost_height[0] = 1;
	ghost_height[1] = 1;

	int ghost_width[2]; // number of ghost in width direction
	ghost_width[0] = 2;
	ghost_width[1] = 2;

	int local_size[2]; // size of the local domain without ghost
	local_size[0] = dsize[0] / psize[0];
	local_size[1] = dsize[1] / psize[1];

	//
	dsize[0]  = local_size[0] + ghost_height[0] + ghost_height[1];
	dstart[0] = ghost_height[0];
	dend[0]   = dsize[0] - ghost_height[1];

	//
	dsize[1]  = local_size[1] + ghost_width[0] + ghost_width[1];
	dstart[1] = ghost_width[0];
	dend[1]   = dsize[1] - ghost_width[1];

	int ii = 0;

	PDI_expose("iter", &ii, PDI_OUT);
	PDI_expose("dsize", dsize, PDI_OUT);
	PDI_expose("dstart", dstart, PDI_OUT);
	PDI_expose("dend", dend, PDI_OUT);
	PDI_expose("psize", psize, PDI_OUT);
	PDI_expose("pcoord", pcoord, PDI_OUT);

	int(*cur)[dsize[1]] = malloc(sizeof(int) * dsize[1] * dsize[0]);

	// initialize 
	for (int yy=0; yy<dsize[0]; ++yy) {
		for (int xx=0; xx<dsize[1]; ++xx) {
			cur[yy][xx] = - (pcoord_1d + 1);
		}
	}

	for (int yy=dstart[0]; yy<dend[0]; ++yy) {
		for (int xx=dstart[1]; xx<dend[1]; ++xx) {
			cur[yy][xx] = (pcoord_1d + 1);
		}
	}
	
	// allocate variables for the vertex of the mesh
  	double(*coords_x)[dsize[1]+1] = malloc(sizeof(double)* (dsize[1]+1) * (dsize[0]+1) );
  	double(*coords_y)[dsize[1]+1] = malloc(sizeof(double)* (dsize[1]+1) * (dsize[0]+1) );
    
  	create_coordinate_of_vertices( dsize, dstart, local_size, pcoord, coords_x, coords_y);

	PDI_multi_expose("newiter", "iter", &ii, PDI_INOUT, "main_field", cur, PDI_INOUT, "coords_x", coords_x, PDI_INOUT, "coords_y", coords_y, PDI_INOUT, NULL);

	// free cur
	free(cur);

	PDI_finalize();
	PC_tree_destroy(&conf);

	MPI_Finalize();
}
