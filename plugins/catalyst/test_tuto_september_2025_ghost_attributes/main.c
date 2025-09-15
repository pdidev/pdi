/*******************************************************************************
 * Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies
 * alternatives (CEA)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 ******************************************************************************/

#include <mpi.h>

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <paraconf.h>
#include <pdi.h>

// size of the local data as [HEIGHT, WIDTH] including the number of ghost
// layers for communications or boundary conditions
int dsize[2];

// 2D size of the process grid as [HEIGHT, WIDTH]
int psize[2];

// 2D rank of the local process in the process grid as [YY, XX]
int pcoord[2];

// the alpha coefficient used in the computation
double alpha;

double L = 1.0;
// definition of the source
// the source corresponds to a disk of an uniform value
// source1: center=(0.4,0.4), radius=0.2 and value=100
double source1[4] = {0.4, 0.4, 0.2, 100};
// source2: center=(0.8,0.7), radius=0.1 and value=200
double source2[4] = {0.7, 0.8, 0.1, 200};
// the order of the coordinates of the center (XX,YY) is inverted in the vector

/** Initialize all the data to 0, with the exception of each cells
 *  whose center (cpos_x,cpos_y) is inside of the disks
 *  defined by source1 or source2
 * \param[out] dat the local data to initialize
 */
void init(double dat[dsize[0]][dsize[1]]) {
  for (int yy = 0; yy < dsize[0]; ++yy)
    for (int xx = 0; xx < dsize[1]; ++xx)
      dat[yy][xx] = 0;
  double dy = L / ((dsize[0] - 2) * psize[0]);
  double dx = L / ((dsize[1] - 2) * psize[1]);

  double cpos_x, cpos_y;
  double square_dist1, square_dist2;
  for (int yy = 0; yy < dsize[0]; ++yy) {
    cpos_y = (yy + pcoord[0] * (dsize[0] - 2)) * dy - 0.5 * dy;
    for (int xx = 0; xx < dsize[1]; ++xx) {
      cpos_x = (xx + pcoord[1] * (dsize[1] - 2)) * dx - 0.5 * dx;
      square_dist1 = (cpos_y - source1[0]) * (cpos_y - source1[0]) +
                     (cpos_x - source1[1]) * (cpos_x - source1[1]);
      if (square_dist1 <= source1[2] * source1[2]) {
        dat[yy][xx] = source1[3];
      }
      square_dist2 = (cpos_y - source2[0]) * (cpos_y - source2[0]) +
                     (cpos_x - source2[1]) * (cpos_x - source2[1]);
      if (square_dist2 <= source2[2] * source2[2]) {
        dat[yy][xx] = source2[3];
      }
    }
  }
}

/** Compute the values at the next time-step based on the values at the current
 * time-step
 * \param[in]  cur  the local data at the current time-step
 * \param[out] next the local data at the next    time-step
 */
void iter(double cur[dsize[0]][dsize[1]], double next[dsize[0]][dsize[1]]) {
  int xx, yy;
  for (yy = 1; yy < dsize[0] - 1; ++yy) {
    for (xx = 1; xx < dsize[1] - 1; ++xx) {
      next[yy][xx] = (1. - 4. * alpha) * cur[yy][xx] +
                     alpha * (cur[yy][xx - 1] + cur[yy][xx + 1] +
                              cur[yy - 1][xx] + cur[yy + 1][xx]);
    }
  }
}

/** Exchange ghost values with neighbours
 * \param[in] cart_comm the MPI communicator with all processes organized in a
 * 2D Cartesian grid
 * \param[in] cur the local data at the current time-step
 * whose ghosts need exchanging
 */
void exchange(MPI_Comm cart_comm, double cur[dsize[0]][dsize[1]]) {
  MPI_Status status;
  int rank_source, rank_dest;
  static MPI_Datatype column, row;
  static int initialized = 0;

  if (!initialized) {
    MPI_Type_vector(dsize[0] - 2, 1, dsize[1], MPI_DOUBLE, &column);
    MPI_Type_commit(&column);
    MPI_Type_contiguous(dsize[1] - 2, MPI_DOUBLE, &row);
    MPI_Type_commit(&row);
    initialized = 1;
  }

  // send down
  MPI_Cart_shift(cart_comm, 0, 1, &rank_source, &rank_dest);
  MPI_Sendrecv(&cur[dsize[0] - 2][1], 1, row, rank_dest, 100, // send row before ghost
               &cur[0][1],            1, row, rank_source, 100, // receive 1st row (ghost)
               cart_comm, &status);

  // send up
  MPI_Cart_shift(cart_comm, 0, -1, &rank_source, &rank_dest);
  MPI_Sendrecv(&cur[1][1],            1, row, rank_dest, 100, // send row after ghost
               &cur[dsize[0] - 1][1], 1, row, rank_source, 100, // receive last row (ghost)
               cart_comm, &status);

  // send to the right
  MPI_Cart_shift(cart_comm, 1, 1, &rank_source, &rank_dest);
  MPI_Sendrecv(&cur[1][dsize[1] - 2], 1, column, rank_dest, 100, // send column before ghost
               &cur[1][0],            1, column, rank_source, 100, // receive 1st column (ghost)
               cart_comm, &status);

  // send to the left
  MPI_Cart_shift(cart_comm, 1, -1, &rank_source, &rank_dest);
  MPI_Sendrecv(&cur[1][1],            1, column, rank_dest, 100, // send column after ghost
               &cur[1][dsize[1] - 1], 1, column, rank_source, 100, // receive last column (ghost)
               cart_comm, &status);
}

void create_mesh_and_ghost_type( double coords_x[dsize[0]+1][dsize[1]+1], double coords_y[dsize[0]+1][dsize[1]+1],
                 uint8_t ghosts_type_cells[dsize[0]][dsize[1]]){

  // catalyst variables
  int cells_ghost=1;

  size_t number_of_points[2];
  number_of_points[0] = dsize[0]+1;
  number_of_points[1] = dsize[1]+1;
  size_t total_number_of_points =  number_of_points[0]*number_of_points[1]; 

  // the first axis correspond to the y-coordinate.
  for(int ix=0; ix<number_of_points[0]; ix++) {
    for(int iy=0; iy<number_of_points[1]; iy++) {
      coords_y[ix][iy] = 1.0*(ix-cells_ghost) + pcoord[0]*(dsize[0]-2.0*cells_ghost);
      coords_x[ix][iy] = 1.0*(iy-cells_ghost) + pcoord[1]*(dsize[1]-2.0*cells_ghost);
    } 
  }

  for(int gg=0; gg<dsize[0]; gg++){
      for(int hh=0; hh<dsize[1]; hh++){
        ghosts_type_cells[gg][hh] = 0;
      }
  }

  // value for the ghost element.
  for(int hh=0; hh<dsize[1]; hh++){
    ghosts_type_cells[0][hh] = /*vtkDataSetAttributes::CellGhostTypes::DUPLICATECELL*/ 1;
    ghosts_type_cells[dsize[0]-1][hh] = /*vtkDataSetAttributes::CellGhostTypes::DUPLICATECELL*/ 1;
  }

  for(int gg=0; gg<dsize[0]; gg++){
    ghosts_type_cells[gg][0] = /*vtkDataSetAttributes::CellGhostTypes::DUPLICATECELL*/ 1;
    ghosts_type_cells[gg][dsize[1]-1] = /*vtkDataSetAttributes::CellGhostTypes::DUPLICATECELL*/ 1;
  }
}

int main(int argc, char *argv[]) {
  MPI_Init(&argc, &argv);

  // load the configuration tree
  PC_tree_t conf = PC_parse_path("config.yml");
  PDI_init(PC_get(conf, ".pdi"));
  
  // NEVER USE MPI_COMM_WORLD IN THE CODE, use our own communicator main_comm
  // instead
  MPI_Comm main_comm = MPI_COMM_WORLD;

  // load the MPI rank & size
  int psize_1d;
  MPI_Comm_size(main_comm, &psize_1d);
  int pcoord_1d;
  MPI_Comm_rank(main_comm, &pcoord_1d);

  long longval;

  // load the alpha parameter
  PC_double(PC_get(conf, ".alpha"), &alpha);

  int global_size[2];
  // load the global data-size
  // you can use paraconf to read some parameters from the yml config file
  PC_int(PC_get(conf, ".global_size.height"), &longval);
  global_size[0] = longval;
  PC_int(PC_get(conf, ".global_size.width"), &longval);
  global_size[1] = longval;

  // load the parallelism configuration
  PC_int(PC_get(conf, ".parallelism.height"), &longval);
  psize[0] = longval;
  PC_int(PC_get(conf, ".parallelism.width"), &longval);
  psize[1] = longval;

  // check the configuration is coherent
  assert(global_size[0] % psize[0] == 0);
  assert(global_size[1] % psize[1] == 0);
  assert(psize[1] * psize[0] == psize_1d);

  // compute the local data-size (the number of ghost layers is 2 for each
  // coordinate)
  dsize[0] = global_size[0] / psize[0] + 2;
  dsize[1] = global_size[1] / psize[1] + 2;

  PDI_expose("local_size", dsize, PDI_OUT);
  PDI_expose("rank", &pcoord_1d, PDI_OUT);

  // create a 2D Cartesian MPI communicator & get our coordinate (rank) in it
  int cart_period[2] = {1, 1};
  MPI_Comm cart_comm;
  MPI_Cart_create(main_comm, 2, psize, cart_period, 1, &cart_comm);
  MPI_Cart_coords(cart_comm, pcoord_1d, 2, pcoord);

  // allocate variable for the mesh and ghost type cells
  // Reminder:
  // The number of cells in a subdomain (including ghost cells) is [dsize[0], dsize[1]]
  // The number of points in a subdomain (including ghost cells) is [dsize[0]+1, dsize[1]+1]
  double(*coords_x)[dsize[1]+1] = malloc(sizeof(double)* (dsize[1]+1) * (dsize[0]+1) );
  double(*coords_y)[dsize[1]+1] = malloc(sizeof(double)* (dsize[1]+1) * (dsize[0]+1) );
  uint8_t (*ghost_type_cells)[dsize[1]] = malloc(sizeof(uint8_t) * dsize[0] * dsize[1]);
  
  create_mesh_and_ghost_type( coords_x, coords_y, ghost_type_cells);

  PDI_expose("pcoord", pcoord, PDI_OUT);
  PDI_expose("psize", psize, PDI_OUT);

  // allocate memory for the double buffered data
  double(*cur)[dsize[1]] = malloc(sizeof(double) * dsize[1] * dsize[0]);
  double(*next)[dsize[1]] = malloc(sizeof(double) * dsize[1] * dsize[0]);

  // initialize the data content
  init(cur);

  // our loop counter so as to be able to use it outside the loop
  int ii = 0;
  int max_iter = 2;
  double time=0;
  double dt=1;

  PDI_expose("max_iter", &max_iter, PDI_OUT);

  // the main loop
  for (; ii < max_iter; ++ii) {    
    time=ii*dt;
    PDI_multi_expose("catalyst_execute", "cycle", &ii, PDI_OUT, 
                      "time", &time, PDI_OUT,
                      "temp", cur, PDI_OUT, 
                      "ghosts", ghost_type_cells, PDI_OUT,
                      "coords_x", coords_x, PDI_OUT,
                      "coords_y", coords_y, PDI_OUT,
                      NULL);

    // compute the values for the next iteration
    iter(cur, next);

    // exchange data with the neighbours
    exchange(cart_comm, next);

    // swap the current and next values
    double(*tmp)[dsize[1]] = cur;
    cur = next;
    next = tmp;
  }

  time=ii*dt;
  PDI_multi_expose("catalyst_execute", "cycle", &ii, PDI_OUT, 
                    "time", &time, PDI_OUT,
                    "temp", cur, PDI_OUT,
                    "ghosts", ghost_type_cells, PDI_OUT,
                    "coords_x", coords_x, PDI_OUT,
                    "coords_y", coords_y, PDI_OUT,
                    NULL);

  PDI_finalize();
  // destroy the paraconf configuration tree
  PC_tree_destroy(&conf);

  // free the allocated memory
  free(cur);
  free(next);

  // finalize MPI
  MPI_Finalize();

  fprintf(stderr, "[%d] SUCCESS\n", pcoord_1d);
  return EXIT_SUCCESS;
}
