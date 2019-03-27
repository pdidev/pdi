/****************************************************************************
 **  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
 *****************************************************************************
 **  Copyright (c) 2008-2018                                                **
 **  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
 **                                                                         **
 **  See the file COPYRIGHT in the package base directory for details       **
 ****************************************************************************/

#include <stdio.h>
#include <iostream>
#include <string>
#include <iomanip>
#include <stdlib.h> 
#include <vector>

#include <mpi.h>

#include "sion_cxx.h"

#define ONRANK if(rank==0)

using namespace std;

int main(int argc, char **argv) {

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */
  int rank, procs;

  /* initialize MPI */
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &procs);

   {
    // ----------------------------------
    // TEST A: write some data to file
    // ----------------------------------

    string fname = "cxxtestA.out";
    sionlib::mpi::SIONFile f(fname);

    f.setChunkSize(100);
    f.setFileSystemBlockSize(10);
    f.setGlobalRank(rank);
    f.setNumberOfFiles(1);
 
    f.open();
 
    // Write some data to cxxtestA.out  
    f << rank*3;
    f << rank;
 
    double z = 3.1415*static_cast<double>(rank)+2;
    f << z;

    f.close();
 
    fflush(stderr);
    fflush(stdout);
    MPI_Barrier(MPI_COMM_WORLD);
    ONRANK cout << "on rank " << rank << ": END of TEST A" << endl;
    MPI_Barrier(MPI_COMM_WORLD);
  }

  {
    // ----------------------------------
    // TEST B: read data from file
    // ----------------------------------
    MPI_Barrier(MPI_COMM_WORLD);
    string fname = "cxxtestA.out";
    sionlib::mpi::SIONFile f(fname);
    f.setMode("rb");
    f.setGlobalRank(4);
    f.open();
    // read data from cxxtestA.out
    int int_data; 
    f >> int_data;
    cout << "Data read from rank " << rank << ": " << int_data << endl;
    ONRANK cout << "on rank " << rank << ": read in data: " <<  int_data << endl;

    f >> int_data;
    cout << "Data read from rank " << rank << ": " << int_data << endl;
    ONRANK cout << "on rank " << rank << ": read in data: " << int_data << endl;

    double double_data;
    f >> double_data;
    cout << "Data read from rank " << rank << ": " << double_data << endl;
    ONRANK cout << "on rank " << rank << ": read in data: " << double_data << endl;
    f.close();

    fflush(stderr);
    fflush(stdout);
    MPI_Barrier(MPI_COMM_WORLD);
    ONRANK cout << "on rank " << rank << ": END of TEST B" << endl;
    MPI_Barrier(MPI_COMM_WORLD);
  }

  MPI_Finalize();
  return 0;
}
