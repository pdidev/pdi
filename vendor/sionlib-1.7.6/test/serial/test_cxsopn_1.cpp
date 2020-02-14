/****************************************************************************
 **  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
 *****************************************************************************
 **  Copyright (c) 2008-2019                                                **
 **  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
 **                                                                         **
 **  See the file COPYRIGHT in the package base directory for details       **
 ****************************************************************************/

#include <iostream>
#include <string>
#include <iomanip>
#include <stdlib.h> 
#include <vector>

#include "sion_cxx.h"

using namespace std;

int main() {
   {
    // ----------------------------------
    // TEST A: test with wrong parameters
    // ----------------------------------
    string fname = "cxxtestA.out";

    sionlib::serial::SIONFile f(fname);

    f.setFileSystemBlockSize(10);
    f.setChunkSizes(NULL);
    f.setNumberOfTasks(4);
    f.setNumberOfFiles(1);

    f.open();

    int outsid = f.getSid();
    if(outsid >= 0) {
      f.close();
    }  else {
      cerr << "TEST A: sion_open returned " << outsid << endl;
    }
  }
  // ----------------------------------
  // TEST B: create an empty file
  // ----------------------------------
   if(0) {
    string fname2 = "cxxtestB.out";
    sionlib::serial::SIONFile f2(fname2);
    //    cout << "Test" << endl;
    f2.setFileSystemBlockSize(10);
    f2.setChunkSizes(NULL);
    f2.setNumberOfTasks(4);
    f2.setNumberOfFiles(1);
    f2.setGlobalRanks(NULL);

    sion_int64 * chunk_sizes = new sion_int64[f2.getNumberOfTasks()];

    f2.setChunkSizes(chunk_sizes);

    if (chunk_sizes == NULL) {
      cerr << "TEST B: cannot allocate chunksizes of size " << f2.getChunkSizes() << ", aborting ...\n";
      return(1);
    }

    int * global_ranks = new int[f2.getNumberOfTasks()];
    f2.setGlobalRanks(global_ranks);
    if (global_ranks== NULL) {
      cerr << "TEST B: cannot allocate globalranks size " << f2.getGlobalRanks() << ", aborting ...\n";
      return(1);
    }

    int ntasks = f2.getNumberOfTasks();
    for(int t=0;t<ntasks;t++) {
      chunk_sizes[t]=1000+t*256;
      global_ranks[t]=t;
    }
    f2.setChunkSizes(chunk_sizes);
    f2.setGlobalRanks(global_ranks);

    f2.open();
    int outsid = f2.getSid();
    if(outsid>=0) {
      f2.close();
    } else {
      cerr << "TEST B: sion_open returned " << outsid << endl;
    }

    delete [] chunk_sizes;
    chunk_sizes = NULL;
    delete [] global_ranks;
    global_ranks = NULL;
  }

  /* ------------------------------------- */
  /* TEST C: create an file with some data */
  /* ------------------------------------- */
   if(0){
#define BUFSIZE 10000  
    string fname3 = "cxxtestC.out";
    sionlib::serial::SIONFile f3(fname3);
    f3.setFileSystemBlockSize(10);
    f3.setChunkSizes(NULL);
    f3.setNumberOfTasks(4);
    f3.setNumberOfFiles(1);
    f3.setGlobalRanks(NULL);

    sion_int64 * chunk_sizes = new sion_int64[f3.getNumberOfTasks()];
    f3.setChunkSizes(chunk_sizes);
    if (chunk_sizes == NULL) {
      cerr << "TEST C: cannot allocate chunksizes of size " << f3.getChunkSizes() << ", aborting ...\n";
      return(1);
    }

    int * global_ranks = new int[f3.getNumberOfTasks()];
    f3.setGlobalRanks(global_ranks);
    if (global_ranks== NULL) {
      cerr << "TEST C: cannot allocate globalranks size " << f3.getGlobalRanks() << ", aborting ...\n";
      return(1);
    }

    int ntasks = f3.getNumberOfTasks();
    for(int t=0;t<ntasks;t++) {
      chunk_sizes[t]=1000+t*256;
      global_ranks[t]=t;
    }
    f3.setChunkSizes(chunk_sizes);
    f3.setGlobalRanks(global_ranks);
    f3.open();
    int outsid = f3.getSid();
    char buffer[BUFSIZE];
    // vector<char> buffer(BUFSIZE);

    if(outsid>=0) {

      /* first write */
      for(int t=0;t<ntasks;t++) {
	for (int i = 0; i < chunk_sizes[t]; i++) buffer[i] = 'A' + t;
	f3.setRank(t);
	f3.seekFp();
	f3.write(buffer);
      }
  
      /* second write */
      for(int t=0;t<ntasks;t++) {
	for (int i = 0; i < chunk_sizes[t]; i++) buffer[i] = 'A' + t;
	f3.setRank(t);
	f3.seekFp();
	f3.write(buffer);
      }
      f3.close();
    } else {
      cerr << "TEST C: sion_open returned " << outsid << endl;
    }

    delete [] chunk_sizes;
    chunk_sizes = NULL;
    delete [] global_ranks;
    global_ranks = NULL;
  }
  
   /* ------------------------------------- */
  /* TEST C: read an file with some data   */
  /* ------------------------------------- */
  if(0) {
    string fname4 = "cxxtestC.out";
    sionlib::serial::SIONFile f4(fname4);
    f4.setNumberOfTasks(4);

    sion_int64 * chunk_sizes = new sion_int64[f4.getNumberOfTasks()];
    if (chunk_sizes == NULL) {
      cerr << "TEST C: cannot allocate chunk_sizes, aborting ...\n";
      return(1);
    }

  int * global_ranks = new int[f4.getNumberOfTasks()];
    if (global_ranks== NULL) {
      cerr << "TEST C: cannot allocate global_ranks size, aborting ...\n";
      return(1);
    }

#define BUFSIZE 10000  
    char   buffer[BUFSIZE];
    int ntasks = 4;
    f4.setNumberOfTasks(ntasks);
    f4.setNumberOfFiles(1);
    f4.setMode("rb");


    for(int t=0;t<ntasks;t++) {
      chunk_sizes[t]=0;
      global_ranks[t]=0;
    }

    f4.setChunkSizes(chunk_sizes);
    f4.setGlobalRanks(global_ranks);

    f4.open();
    int outsid = f4.getSid();
    if(outsid>=0) {
      
      /* skip first read */
      
      /* second read */
      for(int t=0;t<ntasks;t++) {
	for (int i = 0; i < chunk_sizes[t]; i++) buffer[i] = '-';
	f4.setRank(t);
	f4.seekFp();
	f4.read(buffer,1,chunk_sizes[t]);
	cout << "TEST C read: rank=" << t << " chunk_sizes=" << chunk_sizes[t] <<  " rc=" << f4.getReturnCode() << endl;

	// for (int i=0 ; i<chunk_sizes[t]-9 ; ++i) {
	//   cout << buffer[i];
	// }
	// cout << endl;
	//   sion_close(outsid);
      }
    } else {
      fprintf(stderr,"TEST C: sion_open returned %d\n",outsid);
    }
   
    f4.close();

    delete [] chunk_sizes;
    chunk_sizes = NULL;
    delete [] global_ranks;
    global_ranks = NULL;
  }

  /* ------------------------------------- */
  /* TEST D: write data with <<   */
  /* ------------------------------------- */
   if(0)  {

    // Doesn't work yet, why? --> W.F

    string fname5 = "cxxtestD.out";
    sionlib::serial::SIONFile f5(fname5);
    int ntasks = 1;
    f5.setNumberOfTasks(ntasks);
    f5.setNumberOfFiles(1);
    f5.setFileSystemBlockSize(10);
    sion_int64 * chunk_sizes = new sion_int64[f5.getNumberOfTasks()];
    if (chunk_sizes == NULL) {
      cerr << "TEST D: cannot allocate chunk_sizes, aborting ...\n";
      return(1);
    }

    int * global_ranks = new int[f5.getNumberOfTasks()];
    if (global_ranks== NULL) {
      cerr << "TEST D: cannot allocate global_ranks size, aborting ...\n";
      return(1);
    }

    for(int t=0;t<ntasks;t++) {
      chunk_sizes[t]=1000;
      global_ranks[t]=t;
    }

    f5.setChunkSizes(chunk_sizes);
    f5.setGlobalRanks(global_ranks);
    int z=5;
    f5.setRank(0);
    f5.open();
    f5.seekFp();
     f5 << z;
    f5.close();
    // f5.open();
    // f5.setMode("rb");
    // f5.seekFp();
    // int read_value;
    // f5.read(&read_value);
    // cout << "Read in data:\t" << read_value << endl;
    // f5.close();
    delete [] chunk_sizes;
    chunk_sizes = NULL;
    delete [] global_ranks;
    global_ranks = NULL;

  }
  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  return (0);
}
