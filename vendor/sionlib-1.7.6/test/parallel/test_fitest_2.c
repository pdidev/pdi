/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <mpi.h>
#include <time.h>
#include <math.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "sion.h"
#include "sion_debug.h"

#define ONRANK if(rank==0)

char* _sion_buddy_role_to_str (unsigned int flag);


/*

allnodes/testB.out 

siondump: number of files:           5
siondump: file   1:                    testB.out 
siondump: file   2:                    testB.out.000001 
siondump: file   3:                    testB.out.000002 
siondump: file   4:                    testB.out.000003 
siondump: file   5:                    testB.out.000004 
------------------------------------------------------------
  file[000]:  0:00000  1:00001 
  file[001]:  0:00002  1:00003  2:00004 
  file[002]:  0:00005  1:00006  2:00007  3:00008 
  file[003]:  0:00009  1:00010  2:00011  3:00012  4:00013 
  file[004]:  0:00014  1:00015 
------------------------------------------------------------

------------------------------------------------------------
siondump: number of files:           5
siondump: file   1:                    testB.out 
siondump: file   2:                    testB.out.000001 
siondump: file   3:                    testB.out.000002 
siondump: file   4:                    testB.out.000003 
siondump: file   5:                    testB.out.000004 
------------------------------------------------------------
  file[000]:  2:00000  3:00001 
  file[001]:  2:00002  3:00003  4:00004 
  file[002]:  3:00005  4:00006  5:00007  6:00008 
  file[003]:  4:00009  5:00010  6:00011  7:00012  8:00013 
  file[004]:  5:00014  6:00015 
------------------------------------------------------------


 */


int _SCR_Init( int (* check_readable)(char *, void *), void *args) {
  
  int rc;
  char myfilename[]="testB.out";

  rc=check_readable(myfilename, args);
  
  return(rc);
}


int print_io_info(int sid, int rank) {
  
  sion_io_stat_t *p; 
  int c;

  p=sion_get_io_info(sid);
  if(p==NULL) return(0);

  printf("on rank %2d: io_info [nfiles=%d]\n",rank,p->nfiles);
  for(c=0;c<p->nfiles;c++) {
    printf("on rank %2d: io_info [#%02d: fn=%-30s size=%6d role=%-20s]\n",rank,c,p->names[c],(int) p->sizes[c],_sion_buddy_role_to_str(p->roles[c]));
  }
  return(1);
}

/* Similar to test_buddy___1 but using multiple buddy levels */

int main(int argc, char **argv)
{
  int  rank, size;
  char       debugname[80];
  MPI_Comm    lcomm1;
  MPI_Comm    lcomm2;

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);


  /* ------------------------------------------------------------- */
  /* TEST B: write a small file with some different pattern (Coll) */
  /* ------------------------------------------------------------- */
  if(1){
#define BUFSIZE 1000 
#define CHUNKSIZE 1200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 100;
    int         globalrank= rank;
    int         numfiles  = -1;
    char       *newfname=NULL, dirname[80];
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum, gsum; 
    size_t      bwrote;
    int         i, filenum, lrank, lsize;
    

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm1);
    MPI_Comm_size(lcomm1, &lsize);
    MPI_Comm_rank(lcomm1, &lrank);

    sprintf(dirname,"node%03d",filenum);
    if(lrank==0) {
      mkdir(dirname,0751);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_write", rank);
    sion_debug_on(1023,debugname);

    sid = sion_paropen_mpi("testB.out", "bw,buddy=2", &numfiles, MPI_COMM_WORLD, &lcomm1, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {
      sum=0;
      bwrote=sion_coll_fwrite(buffer,1,rank,sid);       for (i = 0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK printf("on rank %2d: bwrote=%5d overall blocksum=%8d \n",rank, (int) bwrote,(int) sum);
      bwrote=sion_coll_fwrite(buffer,1,BUFSIZE/2,sid);     for (i = 0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK printf("on rank %2d: bwrote=%5d overall blocksum=%8d \n",rank, (int) bwrote,(int) sum);
      bwrote=sion_coll_fwrite(buffer,1,BUFSIZE,sid);       for (i = 0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK printf("on rank %2d: bwrote=%5d overall blocksum=%8d \n",rank, (int) bwrote,(int) sum);
      
      bytes_written=sion_get_bytes_written(sid);
      ONRANK printf("on rank %2d: bytes_written=%lld\n",rank,bytes_written);

      print_io_info(sid,rank);
     
      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %2d: error sid = %d\n",rank,sid);
    }
    MPI_Reduce(&sum, &gsum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    ONRANK printf("on rank %2d: global blocksum = %d\n",rank,(int) gsum);

    chdir("..");

  }

#undef BUFSIZE
#undef CHUNKSIZE

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST B write coll\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------------ */
  /* TEST C: check restart, all files there                                   */
  /* ------------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    int         numfiles  = 1;
    char        dirname[80];
    int         rc;
    int         filenum, lrank, lsize;
    sion_file_check_par_args_mpi *buddy_args;


   
    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm2);
    MPI_Comm_size(lcomm2, &lsize);
    MPI_Comm_rank(lcomm2, &lrank);

    sprintf(dirname,"node%03d",filenum);
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_check_C", rank);
    sion_debug_on(1023,debugname);

    /* SCR checks if file can be opened by SIONlib with a callback function which is provided by SIONlib */
    buddy_args = sion_file_check_par_args_init_mpi("br,buddy=2",MPI_COMM_WORLD,numfiles,lcomm2);
    rc=_SCR_Init(&sion_file_check_par_cb_mpi,(void *) buddy_args);
    ONRANK printf("on rank %2d: _SCR_Init returns %d\n",rank, rc);
    sion_file_check_par_args_free_mpi(buddy_args);

    chdir("..");

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST C read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);



  /* ------------------------------------------------------------------------ */
  /* TEST D: check restart, fallback on first buddy (on node 2)               */
  /* ------------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    int         numfiles  = 1;
    char        dirname[80];
    int         rc;
    int         filenum, lrank, lsize;
    sion_file_check_par_args_mpi *buddy_args;

   
    if(rank==0) {
      rename("./node002/testB.out.000002", "./node002/testB.out.000002_deleted"); 
    }

    
    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm2);
    MPI_Comm_size(lcomm2, &lsize);
    MPI_Comm_rank(lcomm2, &lrank);

    sprintf(dirname,"node%03d",filenum);
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_check_D", rank);
    sion_debug_on(1023,debugname);

    /* SCR checks if file can be opened by SIONlib with a callback function which is provided by SIONlib */
    buddy_args = sion_file_check_par_args_init_mpi("br,buddy=2",MPI_COMM_WORLD,numfiles,lcomm2);
    rc=_SCR_Init(&sion_file_check_par_cb_mpi,(void *) buddy_args);
    ONRANK printf("on rank %2d: _SCR_Init returns %d\n",rank, rc);
    sion_file_check_par_args_free_mpi(buddy_args);

    chdir("..");

    if(rank==0) {
      rename("./node002/testB.out.000002_deleted", "./node002/testB.out.000002"); 
    }

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST D read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);




  /* ------------------------------------------------------------------------ */
  /* TEST E: check restart, fallback on second buddy (on node 2)              */
  /* ------------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    int         numfiles  = 1;
    char        dirname[80];
    int         rc;
    int         filenum, lrank, lsize;
    sion_file_check_par_args_mpi *buddy_args;

   
    if(rank==0) {
      rename("./node002/testB.out.000002", "./node002/testB.out.000002_deleted"); 
      rename("./node001/testB.out_BUDDY_00.000002", "./node001/testB.out_BUDDY_00.000002_deleted"); 
    }

    
    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm2);
    MPI_Comm_size(lcomm2, &lsize);
    MPI_Comm_rank(lcomm2, &lrank);

    sprintf(dirname,"node%03d",filenum);
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_check_E", rank);
    sion_debug_on(1023,debugname);

    /* SCR checks if file can be opened by SIONlib with a callback function which is provided by SIONlib */
    buddy_args = sion_file_check_par_args_init_mpi("br,buddy=2",MPI_COMM_WORLD,numfiles,lcomm2);
    rc=_SCR_Init(&sion_file_check_par_cb_mpi,(void *) buddy_args);
    ONRANK printf("on rank %2d: _SCR_Init returns %d\n",rank, rc);
    sion_file_check_par_args_free_mpi(buddy_args);

    chdir("..");

    if(rank==0) {
      rename("./node002/testB.out.000002_deleted", "./node002/testB.out.000002"); 
      rename("./node001/testB.out_BUDDY_00.000002_deleted", "./node001/testB.out_BUDDY_00.000002"); 
     }

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST E read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------------ */
  /* TEST F: check restart, no fallback possible (on node 2)                  */
  /* ------------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    int         numfiles  = 1;
    char        dirname[80];
    int         rc;
    int         filenum, lrank, lsize;
    sion_file_check_par_args_mpi *buddy_args;

   
    if(rank==0) {
      rename("./node002/testB.out.000002", "./node002/testB.out.000002_deleted"); 
      rename("./node001/testB.out_BUDDY_00.000002", "./node001/testB.out_BUDDY_00.000002_deleted"); 
      rename("./node000/testB.out_BUDDY_01.000002", "./node000/testB.out_BUDDY_01.000002_deleted"); 
    }

    
    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm2);
    MPI_Comm_size(lcomm2, &lsize);
    MPI_Comm_rank(lcomm2, &lrank);

    sprintf(dirname,"node%03d",filenum);
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_check_F", rank);
    sion_debug_on(1023,debugname);

    /* SCR checks if file can be opened by SIONlib with a callback function which is provided by SIONlib */
    buddy_args = sion_file_check_par_args_init_mpi("br,buddy=2",MPI_COMM_WORLD,numfiles,lcomm2);
    rc=_SCR_Init(&sion_file_check_par_cb_mpi,(void *) buddy_args);
    ONRANK printf("on rank %2d: _SCR_Init returns %d\n",rank, rc);
    sion_file_check_par_args_free_mpi(buddy_args);

    chdir("..");
    if(rank==0) {
      rename("./node002/testB.out.000002_deleted", "./node002/testB.out.000002"); 
      rename("./node001/testB.out_BUDDY_00.000002_deleted", "./node001/testB.out_BUDDY_00.000002"); 
      rename("./node000/testB.out_BUDDY_01.000002_deleted", "./node000/testB.out_BUDDY_01.000002"); 
     }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST F read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------------ */
  /* TEST G: check restart, fallback on first buddy (on node 0)               */
  /* ------------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    int         numfiles  = 1;
    char        dirname[80];
    int         rc;
    int         filenum, lrank, lsize;
    sion_file_check_par_args_mpi *buddy_args;

   
    if(rank==0) {
      rename("./node000/testB.out", "./node000/testB.out_deleted"); 
    }

    
    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm2);
    MPI_Comm_size(lcomm2, &lsize);
    MPI_Comm_rank(lcomm2, &lrank);

    sprintf(dirname,"node%03d",filenum);
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_check_G", rank);
    sion_debug_on(1023,debugname);

    /* SCR checks if file can be opened by SIONlib with a callback function which is provided by SIONlib */
    buddy_args = sion_file_check_par_args_init_mpi("br,buddy=2",MPI_COMM_WORLD,numfiles,lcomm2);
    rc=_SCR_Init(&sion_file_check_par_cb_mpi,(void *) buddy_args);
    ONRANK printf("on rank %2d: _SCR_Init returns %d\n",rank, rc);
    sion_file_check_par_args_free_mpi(buddy_args);

    chdir("..");

    if(rank==0) {
      rename("./node000/testB.out_deleted", "./node000/testB.out"); 
    }

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST G read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------------ */
  /* TEST H: check restart, fallback on second buddy (on node 0)              */
  /* ------------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    int         numfiles  = 1;
    char        dirname[80];
    int         rc;
    int         filenum, lrank, lsize;
    sion_file_check_par_args_mpi *buddy_args;

   
    if(rank==0) {
      rename("./node000/testB.out", "./node000/testB.out_deleted"); 
      rename("./node004/testB.out_BUDDY_00", "./node004/testB.out_BUDDY_00_deleted"); 
    }

    
    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm2);
    MPI_Comm_size(lcomm2, &lsize);
    MPI_Comm_rank(lcomm2, &lrank);

    sprintf(dirname,"node%03d",filenum);
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_check_H", rank);
    sion_debug_on(1023,debugname);

    /* SCR checks if file can be opened by SIONlib with a callback function which is provided by SIONlib */
    buddy_args = sion_file_check_par_args_init_mpi("br,buddy=2",MPI_COMM_WORLD,numfiles,lcomm2);
    rc=_SCR_Init(&sion_file_check_par_cb_mpi,(void *) buddy_args);
    ONRANK printf("on rank %2d: _SCR_Init returns %d\n",rank, rc);
    sion_file_check_par_args_free_mpi(buddy_args);

    chdir("..");

    if(rank==0) {
      rename("./node000/testB.out_deleted", "./node000/testB.out"); 
      rename("./node004/testB.out_BUDDY_00_deleted", "./node004/testB.out_BUDDY_00"); 
     }

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST H read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------------ */
  /* TEST I: check restart, no fallback possible (on node 0)                  */
  /* ------------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    int         numfiles  = 1;
    char        dirname[80];
    int         rc;
    int         filenum, lrank, lsize;
    sion_file_check_par_args_mpi *buddy_args;

   
    if(rank==0) {
      rename("./node000/testB.out", "./node000/testB.out_deleted"); 
      rename("./node004/testB.out_BUDDY_00", "./node004/testB.out_BUDDY_00_deleted"); 
      rename("./node003/testB.out_BUDDY_01", "./node003/testB.out_BUDDY_01_deleted"); 
    }

    
    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm2);
    MPI_Comm_size(lcomm2, &lsize);
    MPI_Comm_rank(lcomm2, &lrank);

    sprintf(dirname,"node%03d",filenum);
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_check_I", rank);
    sion_debug_on(1023,debugname);

    /* SCR checks if file can be opened by SIONlib with a callback function which is provided by SIONlib */
    buddy_args = sion_file_check_par_args_init_mpi("br,buddy=2",MPI_COMM_WORLD,numfiles,lcomm2);
    rc=_SCR_Init(&sion_file_check_par_cb_mpi,(void *) buddy_args);
    ONRANK printf("on rank %2d: _SCR_Init returns %d\n",rank, rc);
    sion_file_check_par_args_free_mpi(buddy_args);

    chdir("..");
    if(rank==0) {
      rename("./node000/testB.out_deleted", "./node000/testB.out"); 
      rename("./node004/testB.out_BUDDY_00_deleted", "./node004/testB.out_BUDDY_00"); 
      rename("./node003/testB.out_BUDDY_01_deleted", "./node003/testB.out_BUDDY_01"); 
     }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST I read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------------ */
  /* TEST J: check restart, fallback possible (on node 2 on buddy level 0     */
  /*         and 1 and node 0 [master file] on buddy level 2).                */
  /*         This should NOT fail, since all relevant data is available.      */
  /* ------------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    int         numfiles  = 1;
    char        dirname[80];
    int         rc;
    int         filenum, lrank, lsize;
    sion_file_check_par_args_mpi *buddy_args;

   
    if(rank==0) {
      rename("./node002/testB.out.000002", "./node002/testB.out.000002_deleted"); 
      rename("./node001/testB.out_BUDDY_00.000002", "./node001/testB.out_BUDDY_00.000002_deleted"); 
      rename("./node003/testB.out_BUDDY_01", "./node003/testB.out_BUDDY_01_deleted"); 
    }

    
    /* filenum=rank/4; */
    if(rank<2) filenum=0;
    else if(rank<5) filenum=1; 
    else if(rank<9) filenum=2; 
    else if(rank<14) filenum=3; 
    else filenum=4; 
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm2);
    MPI_Comm_size(lcomm2, &lsize);
    MPI_Comm_rank(lcomm2, &lrank);

    sprintf(dirname,"node%03d",filenum);
    MPI_Barrier(MPI_COMM_WORLD);
    chdir(dirname);
    MPI_Barrier(MPI_COMM_WORLD);

    sprintf(debugname, "%s.%05d", "deb_buddy_check_J", rank);
    sion_debug_on(1023,debugname);

    /* SCR checks if file can be opened by SIONlib with a callback function which is provided by SIONlib */
    buddy_args = sion_file_check_par_args_init_mpi("br,buddy=2",MPI_COMM_WORLD,numfiles,lcomm2);
    rc=_SCR_Init(&sion_file_check_par_cb_mpi,(void *) buddy_args);
    ONRANK printf("on rank %2d: _SCR_Init returns %d\n",rank, rc);
    sion_file_check_par_args_free_mpi(buddy_args);

    chdir("..");
    if(rank==0) {
      rename("./node002/testB.out.000002_deleted", "./node002/testB.out.000002"); 
      rename("./node001/testB.out_BUDDY_00.000002_deleted", "./node001/testB.out_BUDDY_00.000002"); 
      rename("./node003/testB.out_BUDDY_01_deleted", "./node003/testB.out_BUDDY_01"); 
     }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST J read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();
  
  return(0);
  
}
