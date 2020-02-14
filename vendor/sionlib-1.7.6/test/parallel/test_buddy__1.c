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

  /* -------------------------- */
  /* TEST A: write a empty file */
  /* -------------------------- */
  if(0){
    sion_int64  chunksize = 100;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;

    sid = sion_paropen_mpi("testA.out", "bw,buddy", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) sion_parclose_mpi(sid);

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST A\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------- */
  /* TEST A1: write a small file with some different pattern (Coll) */
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

    sid = sion_paropen_mpi("testB.out", "bw,buddy", &numfiles, MPI_COMM_WORLD, &lcomm1, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
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


  /* ------------------------------------------------------ */
  /* TEST C: read small file from original files       */
  /* ------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL, dirname[80];
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum,gsum; 
    int         i,filenum, b, lrank, lsize;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    
    if(rank==0) {
      /* rename("./node002/testB.out.000002", "./node002/testB.out.000002_deleted"); */
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

    sprintf(debugname, "%s.%05d", "deb_buddy_read_C", rank);
    sion_debug_on(1023,debugname);


    sid = sion_paropen_mpi("testB.out", "br,buddy", &numfiles, MPI_COMM_WORLD, &lcomm2, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %2d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %2d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %2d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %2d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %2d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {
      b=0;
      sum=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %2d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0; i < bread; i++) sum=sum+buffer[i];
	printf("on rank %2d: block=%d           bread=%3d overall blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }             

      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %2d: error sid = %d\n",rank,sid);
    }

    chdir("..");

    MPI_Reduce(&sum, &gsum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    ONRANK printf("on rank %2d: global blocksum = %d\n",rank,(int) gsum);

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST C read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------ */
  /* TEST D: read small file partly from buddy files        */
  /* ------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL, dirname[80];
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum,gsum; 
    int         i,filenum, b, lrank, lsize;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    
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

    sprintf(debugname, "%s.%05d", "deb_buddy_read_D", rank);
    sion_debug_on(1023,debugname);


    sid = sion_paropen_mpi("testB.out", "br,buddy", &numfiles, MPI_COMM_WORLD, &lcomm2, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %2d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %2d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %2d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %2d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %2d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {
      b=0;
      sum=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %2d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0; i < bread; i++) sum=sum+buffer[i];
	printf("on rank %2d: block=%d           bread=%3d overall blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }             

      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %2d: error sid = %d\n",rank,sid);
    }

    chdir("..");

    MPI_Reduce(&sum, &gsum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    ONRANK printf("on rank %2d: global blocksum = %d\n",rank,(int) gsum);

    if(rank==0) {
       rename("./node002/testB.out.000002_deleted", "./node002/testB.out.000002"); 
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST D read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------------ */
  /* TEST E: read small file partly from buddy files        */
  /* ------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL, dirname[80];
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum,gsum; 
    int         i,filenum, b, lrank, lsize;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    
    if(rank==0) {
       rename("./node002/testB.out.000002", "./node002/testB.out.000002_deleted"); 
       rename("./node001/testB.out_BUDDY_00.000002", "./node002/testB.out_BUDDY_00.000002"); 
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

    sprintf(debugname, "%s.%05d", "deb_buddy_read_E", rank);
    sion_debug_on(1023,debugname);


    sid = sion_paropen_mpi("testB.out", "br,buddy", &numfiles, MPI_COMM_WORLD, &lcomm2, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %2d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %2d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %2d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %2d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %2d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {
      b=0;
      sum=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %2d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0; i < bread; i++) sum=sum+buffer[i];
	printf("on rank %2d: block=%d           bread=%3d overall blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }             

      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %2d: error sid = %d\n",rank,sid);
    }

    chdir("..");

    MPI_Reduce(&sum, &gsum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    ONRANK printf("on rank %2d: global blocksum = %d\n",rank,(int) gsum);

    if(rank==0) {
       rename("./node002/testB.out.000002_deleted", "./node002/testB.out.000002"); 
       rename("./node002/testB.out_BUDDY_00.000002", "./node001/testB.out_BUDDY_00.000002"); 
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST E read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------ */
  /* TEST F: restart completely from buddy files            */
  /* ------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL, dirname[80];
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum,gsum; 
    int         i,filenum, b, lrank, lsize;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    
    if(rank==0) {
       rename("./node000/testB.out",        "./node000/testB.out_deleted"); 
       rename("./node001/testB.out.000001", "./node001/testB.out.000001_deleted"); 
       rename("./node002/testB.out.000002", "./node002/testB.out.000002_deleted"); 
       rename("./node003/testB.out.000003", "./node003/testB.out.000003_deleted"); 
       rename("./node004/testB.out.000004", "./node003/testB.out.000004_deleted"); 
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

    sprintf(debugname, "%s.%05d", "deb_buddy_read_F", rank);
    sion_debug_on(1023,debugname);


    sid = sion_paropen_mpi("testB.out", "br,buddy", &numfiles, MPI_COMM_WORLD, &lcomm2, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %2d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %2d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %2d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %2d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %2d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {
      b=0;
      sum=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %2d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0; i < bread; i++) sum=sum+buffer[i];
	printf("on rank %2d: block=%d           bread=%3d overall blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }             

      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %2d: error sid = %d\n",rank,sid);
    }

    chdir("..");

    MPI_Reduce(&sum, &gsum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    ONRANK printf("on rank %2d: global blocksum = %d\n",rank,(int) gsum);

    if(rank==0) {
      rename("./node000/testB.out_deleted"       , "./node000/testB.out"       ); 
      rename("./node001/testB.out.000001_deleted", "./node001/testB.out.000001"); 
      rename("./node002/testB.out.000002_deleted", "./node002/testB.out.000002"); 
      rename("./node003/testB.out.000003_deleted", "./node003/testB.out.000003"); 
      rename("./node003/testB.out.000004_deleted", "./node004/testB.out.000004"); 
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST F read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------ */
  /* TEST G: all original files are on one node             */
  /* ------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL, dirname[80];
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum,gsum; 
    int         i,filenum, b, lrank, lsize;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    
    if(rank==0) {
       rename("./node000/testB.out",        "./node004/testB.out"); 
       rename("./node001/testB.out.000001", "./node004/testB.out.000001"); 
       rename("./node002/testB.out.000002", "./node004/testB.out.000002"); 
       rename("./node003/testB.out.000003", "./node004/testB.out.000003"); 
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

    sprintf(debugname, "%s.%05d", "deb_buddy_read_G", rank);
    sion_debug_on(1023,debugname);


    sid = sion_paropen_mpi("testB.out", "br,buddy", &numfiles, MPI_COMM_WORLD, &lcomm2, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %2d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %2d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %2d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %2d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %2d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {
      b=0;
      sum=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %2d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0; i < bread; i++) sum=sum+buffer[i];
	printf("on rank %2d: block=%d           bread=%3d overall blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }             

      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %2d: error sid = %d\n",rank,sid);
    }

    chdir("..");

    MPI_Reduce(&sum, &gsum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    ONRANK printf("on rank %2d: global blocksum = %d\n",rank,(int) gsum);

    if(rank==0) {
       rename("./node004/testB.out",        "./node000/testB.out"); 
       rename("./node004/testB.out.000001", "./node001/testB.out.000001"); 
       rename("./node004/testB.out.000002", "./node002/testB.out.000002"); 
       rename("./node004/testB.out.000003", "./node003/testB.out.000003"); 
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST G read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------------ */
  /* TEST H: all buddy files are on one node                */
  /* ------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 1200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL, dirname[80];
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum,gsum; 
    int         i,filenum, b, lrank, lsize;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    
    if(rank==0) {
       rename("./node000/testB.out",                "./node000/testB.out_deleted"        ); 
       rename("./node001/testB.out.000001",         "./node001/testB.out.000001_deleted" ); 
       rename("./node002/testB.out.000002",         "./node002/testB.out.000002_deleted" ); 
       rename("./node003/testB.out.000003",         "./node003/testB.out.000003_deleted" ); 
       rename("./node004/testB.out.000004",         "./node003/testB.out.000004_deleted" ); 
       rename("node000/testB.out_BUDDY_00.000001",  "node003/testB.out_BUDDY_00.000001"  );
       rename("node001/testB.out_BUDDY_00.000002",  "node003/testB.out_BUDDY_00.000002"  );
       rename("node002/testB.out_BUDDY_00.000003",  "node003/testB.out_BUDDY_00.000003"  );
       rename("node004/testB.out_BUDDY_00",         "node003/testB.out_BUDDY_00"         );
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

    sprintf(debugname, "%s.%05d", "deb_buddy_read_G", rank);
    sion_debug_on(1023,debugname);


    sid = sion_paropen_mpi("testB.out", "br,buddy", &numfiles, MPI_COMM_WORLD, &lcomm2, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %2d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %2d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %2d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %2d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %2d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {
      b=0;
      sum=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %2d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0; i < bread; i++) sum=sum+buffer[i];
	printf("on rank %2d: block=%d           bread=%3d overall blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }             

      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %2d: error sid = %d\n",rank,sid);
    }

    chdir("..");

    MPI_Reduce(&sum, &gsum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    ONRANK printf("on rank %2d: global blocksum = %d\n",rank,(int) gsum);

    if(rank==0) {
      rename("./node000/testB.out_deleted"       ,"./node000/testB.out"                 ); 
      rename("./node001/testB.out.000001_deleted","./node001/testB.out.000001"          ); 
      rename("./node002/testB.out.000002_deleted","./node002/testB.out.000002"          ); 
      rename("./node003/testB.out.000003_deleted","./node003/testB.out.000003"          ); 
      rename("./node003/testB.out.000004_deleted","./node004/testB.out.000004"          ); 
      rename("node003/testB.out_BUDDY_00.000001" ,"node000/testB.out_BUDDY_00.000001"   );
      rename("node003/testB.out_BUDDY_00.000002" ,"node001/testB.out_BUDDY_00.000002"   );
      rename("node003/testB.out_BUDDY_00.000003" ,"node002/testB.out_BUDDY_00.000003"   );
      rename("node003/testB.out_BUDDY_00"        ,"node004/testB.out_BUDDY_00"          );
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %2d: END of TEST H read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();
  
  return(0);
  
}
