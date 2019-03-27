/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <mpi.h>

#include <time.h>
#include <math.h>


#include "sion.h"
#include "sion_debug.h"
#include "sion_printts.h"
#include "ompi_partest.h"

#include <omp.h>



#define _PARTEST_SION_INT32     10
#define _PARTEST_SION_INT64     11
#define _PARTEST_DOUBLE    12



/****************************************************************************************************************
 *
 *                      test_paropen_multi_omp
 *
 ***************************************************************************************************************/

/*!\brief test
 *
 * @param[in] *filename					filename of the test file
 * @param[in] *localbuffer 				buffer for written data
 * @param[in] *communicators			struct with all the MPI related stuff
 * @param[in] *options					struct with all the partest specific options
 *
 */


int test_paropen_omp(char *filename,
                           char *localbuffer,
			   _test_communicators *communicators, 
			   _test_options *options
			   ) {

  double    starttime, gopentime, opentime, unlinktime, gwritetime, writetime, gclosetime, closetime, readtime, greadtime;
  double    barr1time, barr2time, barr3time;

  sion_int64 left;
  sion_int64 bsumwrote, sumsize, bsumread;
  double    checksum_fp, checksum_read_fp;
  int       globalrank, sid, i, lstartoffset;
  size_t    bwrite, bwrote, btoread, bread;
  int       chunkcnt;
  sion_int64 rchunksize;
  sion_int32 rfsblksize;
  FILE     *fp;
  int       fd;
  char      cbuffer[2*MAXCHARLEN];
  char      *newfname;
  size_t    bytes_in_chunk; 
  int       myfeof;

  int thread_num = omp_get_thread_num();

  /****************************** WRITE *****************************/

  /*                                    */ starttime = MPI_Wtime();
  sid = sion_paropen_omp(filename, "bw", &options->chunksize, &options->fsblksize, &globalrank, &fp, &newfname);
  /*                                    */ opentime = MPI_Wtime() - starttime;
  /*                                    */ starttime = MPI_Wtime();
  barrier_after_open(communicators->work);
  /*                                    */ barr1time = MPI_Wtime() - starttime;


  checksum_fp = 0;

  if(options->use_posix) {
    fd = fileno(fp);
  }

  left = options->totalsize;
  bsumwrote = 0;
  chunkcnt = 0;
  bytes_in_chunk=0;
  lstartoffset=options->startoffset;
  /* Write until we reach the total size of the data */
  while (left > 0) {
		if(lstartoffset==0) bwrite = options->bufsize;
		else {
		  bwrite = lstartoffset; lstartoffset=0;
		}
		if (bwrite > left) bwrite = left;

		if (((options->debug && thread_num == 0)) || ((options->Debug && (thread_num == omp_get_num_threads() - 1)))) {
		  fprintf(stderr, "timings[t%03d] write %lld bytes\n", thread_num, (sion_int64) bwrite);
		}

		bytes_in_chunk+=bwrite;
		if(bytes_in_chunk>options->chunksize) {
		  sion_ensure_free_space(sid, bwrite);
		  bytes_in_chunk=bwrite;
		}

		if(options->use_posix) {
		  bwrote = write(fd, localbuffer, 1*bwrite);
		} else {
		  bwrote = fwrite(localbuffer, 1, bwrite, fp);
		}

		#ifdef CHECKSUM
			if(!options->suppress_checksum) {
			  checksum_fp=0.0;
			  for (i = 0; i < bwrote; i++)
				checksum_fp += (double) localbuffer[i];
			}
		#endif

		left -= bwrote;
		bsumwrote += bwrote;
		chunkcnt++;


		if (((options->debug && thread_num == 0)) || ((options->Debug && (thread_num == omp_get_num_threads() - 1)))) {
		  fprintf(stderr, "timings[t%03d] wrote (%lld bytes) %lld bytes (%10.4f MB) (%lld left)\n",
				  thread_num,(sion_int64) bwrote, bsumwrote, bsumwrote / 1024.0 / 1024.0, (sion_int64) left);
		  fprintf(stderr, "timings[t%03d] after write   position in file= %lld \n", thread_num, sion_get_position(sid));
		}
  }
  fflush(fp);
  /*                                    */ writetime = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  barrier_after_write(communicators->work);
  /*                                    */ barr2time = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  sion_parclose_omp(sid);
  /*                                    */ closetime = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  barrier_after_close(communicators->work);
  /*                                    */ barr3time = MPI_Wtime() - starttime;


  if (writetime == 0) writetime = -1;

  if (options->verbose) {
		sprintf(cbuffer,
            "timings[t%03d] open=%10.6fs write=%10.6fs close=%10.6fs barrier(open=%10.6fs, write=%10.6fs, close=%10.6fs) #chunks=%d bw=%10.4f MB/s ionode=%d\n",
            thread_num, opentime, writetime, closetime, barr1time, barr2time, barr3time, chunkcnt,
            options->totalsize / 1024.0 / 1024.0 / writetime, communicators->ionode_number);
		collective_print_gather(cbuffer, communicators->work);
  }

  reduce_omp(&bsumwrote,&sumsize,MPI_SUM,_PARTEST_SION_INT64);
  reduce_omp(&opentime,&gopentime,MPI_MAX,_PARTEST_DOUBLE);
  reduce_omp(&closetime,&gclosetime,MPI_MAX,_PARTEST_DOUBLE);
  reduce_omp(&writetime,&gwritetime,MPI_MAX,_PARTEST_DOUBLE);

  #pragma omp master
  {
	  fprintf(stderr, "------------------------------------------------------------------------------------------\n");
	  fprintf(stderr, "TOTAL result: open=%10.6fs close=%10.6fs wrote %10.4f MB write=%10.6fs bw=%10.4f MB/s to %d files\n",
				gopentime, gclosetime, 1.0 * sumsize / 1024.0 / 1024.0, gwritetime, 1.0 * sumsize / 1024.0 / 1024.0 / gwritetime, options->numfiles);
	  fprintf(stderr, "------------------------------------------------------------------------------------------\n");
	  fprintf(stderr, "*********************************************************************************************\n");
  }
#pragma omp barrier

  /****************************** READ *****************************/

  /* reset localbuffer */
  for (i = 0; i < ((options->totalsize < options->bufsize) ? options->totalsize : options->bufsize); i++) {
    localbuffer[i] = ' ';
  }

  /*                                    */ starttime = MPI_Wtime();
  if (options->collectiveopenforread) {

    sid = sion_paropen_omp(filename,"br", &options->chunksize, &options->fsblksize, &globalrank, &fp, &newfname);

  }
  else {
    /* there is some work to for multifile sion file */
    sid = sion_open_rank(filename, "br", &rchunksize, &rfsblksize, &communicators->workread_rank, &fp);
  }
  /*                                    */ opentime = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  barrier_after_open(communicators->workread);
  /*                                    */ barr1time = MPI_Wtime() - starttime;

  if(options->use_posix) {
    fd = fileno(fp);
  }
      
  checksum_read_fp = 0;
  left = options->totalsize;
  bsumread = 0;
  chunkcnt = 0;
  bytes_in_chunk = 0;
  lstartoffset=options->startoffset;

  myfeof=sion_feof(sid);
  while ((left > 0) && (!myfeof)) {
	
	if(lstartoffset==0) btoread = options->bufsize;
	else {
	  btoread = lstartoffset; lstartoffset=0;
	}
	if (btoread > left)
	  btoread = left;
      
	bytes_in_chunk+=btoread;
	if(bytes_in_chunk>options->chunksize) {
	  myfeof=sion_feof(sid);
	}
	if(!myfeof) {
		  if(options->use_posix) {
		    bread = read(fd, localbuffer, 1*btoread);
		  } else {
		    bread = fread(localbuffer, 1, btoread, fp);
		  }

		#ifdef CHECKSUM
			  if(!options->suppress_checksum) {
				checksum_read_fp=0.0;
				for (i = 0; i < bread; i++)
				  checksum_read_fp += (double) localbuffer[i];
			  }
		#endif

		  left -= bread;
		  bsumread += bread;
		  chunkcnt++;
		#pragma omp master
		  {
			  if (((options->debug && thread_num == 0)) || ((options->Debug && (thread_num == omp_get_num_threads() - 1)))) {
				fprintf(stderr, "timings[t%03d] read (%lld bytes) %lld bytes (%10.4f MB) (%lld left)\n",
					thread_num, (sion_int64) bread, bsumread, bsumread / 1024.0 / 1024.0, (sion_int64) left);
				fprintf(stderr, "timings[t%03d] after read   position in file= %lld restinblock=%lld\n",
					thread_num, sion_get_position(sid), sion_bytes_avail_in_block(sid));
			  }
		  }
#pragma omp barrier
	}
  }
  fflush(fp);

  /*                                    */ readtime = MPI_Wtime() - starttime;

  /*                                    */ starttime= MPI_Wtime();
  barrier_after_read(communicators->workread);
  /*                                    */ barr2time = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  if (options->collectiveopenforread) {
    sion_parclose_omp(sid);
  }
  else {
    sion_close(sid);
  }
  /*                                    */ closetime = MPI_Wtime() - starttime;

  barrier_after_close(communicators->workread);

  if (readtime == 0)
    readtime = -1;
  if (options->verbose) {
    sprintf(cbuffer,
            "timings[t%03d] open=%10.6fs read=%10.6fs close=%10.6fs barrier(open=%10.6fs, read=%10.6fs, close=%10.6fs) #chunks=%d br=%10.4f MB/s ionode=%d (check %d)\n",
            thread_num, opentime, readtime, closetime, barr1time, barr2time, barr3time, chunkcnt,
            options->totalsize / 1024.0 / 1024.0 / readtime, communicators->ionode_number, (fabs(checksum_fp - checksum_read_fp) < 1e-5));
	collective_print_gather(cbuffer, communicators->workread);

  }

#ifdef CHECKSUM
  if(!options->suppress_checksum) {
    if (fabs(checksum_fp - checksum_read_fp) > 1e-5) {
      fprintf(stderr, "timings[t%03d] ERROR in double checksum  %14.10f==%14.10f, diff=%14.10f\n", thread_num,
	      checksum_fp, checksum_read_fp, checksum_fp - checksum_read_fp);
    }
  }
#endif

	  reduce_omp(&bsumread,&sumsize,MPI_SUM,_PARTEST_SION_INT64);
	  reduce_omp(&opentime,&gopentime,MPI_MAX,_PARTEST_DOUBLE);
	  reduce_omp(&closetime,&gclosetime,MPI_MAX,_PARTEST_DOUBLE);
	  reduce_omp(&readtime,&greadtime,MPI_MAX,_PARTEST_DOUBLE);

	#pragma omp master
	  {
		fprintf(stderr, "------------------------------------------------------------------------------------------\n");
		fprintf(stderr, "TOTAL result: open=%10.6fs close=%10.6fs read %10.4f MB read=%10.6fs br=%10.4f MB/s from %d files\n",
				gopentime, gclosetime, 1.0 * sumsize / 1024.0 / 1024.0, greadtime, 1.0 * sumsize / 1024.0 / 1024.0 / greadtime, options->numfiles);
		fprintf(stderr, "------------------------------------------------------------------------------------------\n");
	  }
#pragma omp barrier
  
  if(options->unlink_files) {
    /*                                    */ starttime = MPI_Wtime();
    barrier_before_unlink(communicators->workread);
	#pragma omp master
    {
	  fprintf(stderr, "partest result: unlink file %s ...\n", newfname);
	  unlink(newfname);
	}
#pragma omp barrier

    barrier_after_unlink(communicators->workread);
    /*                                    */ unlinktime = MPI_Wtime() - starttime;
	#pragma omp master
    {
	  fprintf(stderr, "partest result:  ultime=%10.6fs unlink %s\n", unlinktime, newfname);
    }
#pragma omp barrier
  }

  return (1);
}
