#ifndef MANDELMPI_H
#define MANDELMPI_H

#include <mpi.h>
#include "infostruct.h"

void calc(int *iterations, int width, int height, double xmin, double xmax, double ymin, double ymax, int maxiter,
          double *calctime);

void calc_master(int *iterations, int width, int height, int numprocs, int *blocksize, int collect, double *calctime,
                 MPI_Datatype *commtype, double *commtime);

void calc_worker(int *iterations, int *io_id, _infostruct *infostruct, int collect, double *iotime, double *calctime,
                 double *commtime);

void open_file(int *io_id, _infostruct *infostruct, int *blocksize, int *start,
               int myid, double *iotime);

void write_to_file(int *io_id, const _infostruct *infostruct, int *iterations,
                   int width, int height, int xpos, int ypos, double *iotime);

void close_file(int *io_id, const _infostruct *infostruct, int myid, double *iotime);

#endif
