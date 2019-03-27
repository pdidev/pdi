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

/****************************************************************************************************************
 *
 *                      test_paropen_multi_ompi
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


int test_paropen_multi_ompi(char *filename,
                           char *localbuffer,
			   _test_communicators *communicators, 
			   _test_options *options
			   ) {

  double    starttime, gopentime, opentime, unlinktime, gwritetime, writetime, gclosetime, closetime, readtime, greadtime;
  double    barr1time, barr2time, barr3time;

  double t_writetime, t_readtime, t_opentime, t_closetime;
  sion_int64 t_bsumread, t_bsumwrote;

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
  int       ser_count,ser_step,ser_done;

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "start");

  /* not working task ? */
  if (communicators->work_size == -1) {
    return (0);
  }

  if(0){

    MPI_Comm_size(communicators->work, &communicators->work_size);
    MPI_Comm_rank(communicators->work, &communicators->work_rank);
    MPI_Comm_size(communicators->workread, &communicators->workread_size);
    MPI_Comm_rank(communicators->workread, &communicators->workread_rank);
    fprintf(stderr, "timings[%03d,t%03d] entering test_paropen_multi_mpi work=%d of %d workread=%d of %d\n",  communicators->all_rank,omp_get_thread_num(),
	    communicators->work_rank, communicators->work_size, 
	    communicators->workread_rank, communicators->workread_size 
	    );
  }

  /****************************** WRITE *****************************/

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[W]before open");
  /*                                    */ starttime = MPI_Wtime();
  /* numfiles>=1 -> sion will split the communicator  */
  /* numfiles<=0 -> communicators->local contain correct local communicator, computed by split_communicator  */
  sid = sion_paropen_ompi(filename, "bw", &options->numfiles, communicators->work, &communicators->local, &options->chunksize, &options->fsblksize, &globalrank, &fp, &newfname);
  /*                                    */ opentime = MPI_Wtime() - starttime;
  /*                                    */ starttime = MPI_Wtime();
  barrier_after_open(communicators->work);
  /*                                    */ barr1time = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[W]after open");

  /* local is computed again by sion_paropen_mpi if numfiles >=1 */

	  MPI_Comm_size(communicators->local, &communicators->local_size);
	  MPI_Comm_rank(communicators->local, &communicators->local_rank);

  checksum_fp = 0;

  if(options->use_posix) {
    fd = fileno(fp);
  }

  if(options->serialize_blocknum>0) ser_step=options->serialize_blocknum;
  else                     ser_step=communicators->local_size;
  ser_done=0;
  for(ser_count=0;ser_count<communicators->local_size;ser_count+=ser_step) {
    if ((!ser_done) && communicators->local_rank<(ser_count+ser_step)) {
      /* fprintf(stderr, "starting write on task %d ser_count=%d ser_step=%d ser_done=%d\n", communicators->local_rank,ser_count,ser_step,ser_done); */
      ser_done=1;
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

			#pragma omp master
			{
				if (((options->debug && communicators->all_rank == 0)) || ((options->Debug && communicators->all_rank == communicators->all_size))) {
				  fprintf(stderr, "timings[%03d,t%03d] write %lld bytes\n", communicators->all_rank,omp_get_thread_num(), (sion_int64) bwrite);
				}
			}
#pragma omp barrier
	
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


			if (((options->debug && communicators->all_rank == 0)) || ((options->Debug && communicators->all_rank == communicators->all_size))) {
			  fprintf(stderr, "timings[%03d,t%03d] wrote (%lld bytes) %lld bytes (%10.4f MB) (%lld left)\n", communicators->all_rank,omp_get_thread_num(),(sion_int64) bwrote,
				  bsumwrote, bsumwrote / 1024.0 / 1024.0, (sion_int64) left);
			  fprintf(stderr, "timings[%03d,t%03d] after write   position in file= %lld \n", communicators->all_rank,omp_get_thread_num(), sion_get_position(sid));
			}
      }
    }

  }
  fflush(fp);

  /*                                    */ writetime = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  barrier_after_write(communicators->work);
  /*                                    */ barr2time = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  sion_parclose_ompi(sid);
  /*                                    */ closetime = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[W]before close");
  /*                                    */ starttime = MPI_Wtime();
  barrier_after_close(communicators->work);
  /*                                    */ barr3time = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[W]after close");

  if (writetime == 0) writetime = -1;

  if (options->verbose) {
		sprintf(cbuffer,
            "timings[%03d,t%03d] open=%10.6fs write=%10.6fs close=%10.6fs barrier(open=%10.6fs, write=%10.6fs, close=%10.6fs) #chunks=%d bw=%10.4f MB/s ionode=%d\n",
            communicators->all_rank,omp_get_thread_num(), opentime, writetime, closetime, barr1time, barr2time, barr3time, chunkcnt,
            options->totalsize / 1024.0 / 1024.0 / writetime, communicators->ionode_number);
		collective_print_gather(cbuffer, communicators->work);

  }

  if (options->numfiles >= 1) {
	  reduce_omp(&bsumwrote,&t_bsumwrote,MPI_SUM,_PARTEST_SION_INT64);
		#pragma omp master
		  {
			  sumsize = 0;
			  MPI_Reduce(&t_bsumwrote, &sumsize, 1, SION_MPI_INT64, MPI_SUM, 0, communicators->local);
			  if (communicators->local_rank == 0) {
				fprintf(stderr, "partest result: local totalsize=%10.4f MB  wrote %10.4f MB to %s all_rank=%d\n", options->totalsize / 1024.0 / 1024.0,
						1.0 * sumsize / 1024.0 / 1024.0, newfname, communicators->all_rank);
			  }
		  }
	#pragma omp barrier
  }

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "before red.");
  reduce_omp(&bsumwrote,&t_bsumwrote,MPI_SUM,_PARTEST_SION_INT64);
  reduce_omp(&opentime,&t_opentime,MPI_MAX,_PARTEST_DOUBLE);
  reduce_omp(&closetime,&t_closetime,MPI_MAX,_PARTEST_DOUBLE);
  reduce_omp(&writetime,&t_writetime,MPI_MAX,_PARTEST_DOUBLE);

  #pragma omp master
  {
	  MPI_Reduce(&t_bsumwrote, &sumsize, 1, SION_MPI_INT64, MPI_SUM, 0, communicators->work);
	  MPI_Reduce(&t_opentime, &gopentime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
	  MPI_Reduce(&t_closetime, &gclosetime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
	  MPI_Reduce(&t_writetime, &gwritetime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
  }
#pragma omp barrier

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "after red.");
	#pragma omp master
  {
	  if (communicators->work_rank == 0) {
		fprintf(stderr, "------------------------------------------------------------------------------------------\n");
		fprintf(stderr, "TOTAL result: open=%10.6fs close=%10.6fs wrote %10.4f MB write=%10.6fs bw=%10.4f MB/s to %d files\n",
				gopentime, gclosetime, 1.0 * sumsize / 1024.0 / 1024.0, gwritetime, 1.0 * sumsize / 1024.0 / 1024.0 / gwritetime, options->numfiles);
		fprintf(stderr, "------------------------------------------------------------------------------------------\n");
	  }
	  if (communicators->work_rank == 0)
		  fprintf(stderr, "*********************************************************************************************\n");
  }
#pragma omp barrier

  /****************************** READ *****************************/

  /* reset localbuffer */
  for (i = 0; i < ((options->totalsize < options->bufsize) ? options->totalsize : options->bufsize); i++) {
    localbuffer[i] = ' ';
  }

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[R]before open");
  /*                                    */ starttime = MPI_Wtime();
  if (options->collectiveopenforread) {
    /* commlocal and numfiles will be read from sion file */
    sid = sion_paropen_ompi(filename,
                           "br", &options->numfiles, communicators->workread, &communicators->local, &options->chunksize, &options->fsblksize, &globalrank, &fp, &newfname);
    /* local is computed again by sion_paropen_mpi if numfiles >=1 */
    MPI_Comm_size(communicators->local, &communicators->local_size);
    MPI_Comm_rank(communicators->local, &communicators->local_rank);

  }
  else {
    /* there is some work to for multifile sion file */
    sid = sion_open_rank(filename, "br", &rchunksize, &rfsblksize, &communicators->workread_rank, &fp);
  }
  /*                                    */ opentime = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  barrier_after_open(communicators->workread);
  /*                                    */ barr1time = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[R]after open");

  if(options->use_posix) {
    fd = fileno(fp);
  }

  if(options->serialize_blocknum>0) ser_step=options->serialize_blocknum;
  else                     ser_step=communicators->local_size;
  ser_done=0;

  for(ser_count=0;ser_count<communicators->local_size;ser_count+=ser_step) {
 
    if ((!ser_done) && communicators->local_rank<(ser_count+ser_step)) {
      /* fprintf(stderr, "starting read on task %d ser_count=%d ser_step=%d ser_done=%d\n", communicators->local_rank,ser_count,ser_step,ser_done); */
      ser_done=1;
      
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
		  if (((options->debug && communicators->all_rank == 0)) || ((options->Debug && communicators->all_rank == communicators->all_size))) {
			fprintf(stderr, "timings[%03d,t%03d] read (%lld bytes) %lld bytes (%10.4f MB) (%lld left)\n",
				communicators->all_rank,omp_get_thread_num(), (sion_int64) bread, bsumread, bsumread / 1024.0 / 1024.0, (sion_int64) left);
			fprintf(stderr, "timings[%03d,t%03d] after read   position in file= %lld restinblock=%lld\n",
				communicators->all_rank,omp_get_thread_num(), sion_get_position(sid), sion_bytes_avail_in_block(sid));
		  }
	  }
#pragma omp barrier
	}
      }
    }

    /* fprintf(stderr, "after barrier on local comm on task %d ser_count=%d ser_step=%d ser_done=%d\n", communicators->local_rank,ser_count,ser_step,ser_done); */
    /* barrier_after_read(communicators->local); */
  }
   fflush(fp);
  /*                                    */ readtime = MPI_Wtime() - starttime;

  /*                                    */ starttime= MPI_Wtime();
  barrier_after_read(communicators->workread);
  /*                                    */ barr2time = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  if (options->collectiveopenforread) {
    sion_parclose_ompi(sid);
  }
  else {
    sion_close(sid);
  }
  /*                                    */ closetime = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[R]before close");
  barrier_after_close(communicators->workread);
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[R]after close");

  if (readtime == 0)
    readtime = -1;
  if (options->verbose) {
    sprintf(cbuffer,
            "timings[%03d,t%03d] open=%10.6fs read=%10.6fs close=%10.6fs barrier(open=%10.6fs, read=%10.6fs, close=%10.6fs) #chunks=%d br=%10.4f MB/s ionode=%d (check %d)\n",
            communicators->all_rank,omp_get_thread_num(), opentime, readtime, closetime, barr1time, barr2time, barr3time, chunkcnt,
            options->totalsize / 1024.0 / 1024.0 / readtime, communicators->ionode_number, (fabs(checksum_fp - checksum_read_fp) < 1e-5));

	collective_print_gather(cbuffer, communicators->workread);

  }

#ifdef CHECKSUM
  if(!options->suppress_checksum) {
    if (fabs(checksum_fp - checksum_read_fp) > 1e-5) {
      fprintf(stderr, "timings[%03d,t%03d] ERROR in double checksum  %14.10f==%14.10f, diff=%14.10f\n", communicators->local_rank,omp_get_thread_num(),
	      checksum_fp, checksum_read_fp, checksum_fp - checksum_read_fp);
    }
  }
#endif

  if (options->numfiles >= 1) {
	  reduce_omp(&bsumread,&t_bsumread,MPI_SUM,_PARTEST_SION_INT64);
	#pragma omp master
    {
		    MPI_Reduce(&t_bsumread, &sumsize, 1, SION_MPI_INT64, MPI_SUM, 0, communicators->local);
		if (communicators->local_rank == 0) {
		  fprintf(stderr, "partest result: read  %10.4f MB from %s\n", 1.0 * sumsize / 1024.0 / 1024.0, newfname);
		}
    }
#pragma omp barrier
  }

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "before red.");
	  reduce_omp(&bsumread,&t_bsumread,MPI_SUM,_PARTEST_SION_INT64);
	  reduce_omp(&opentime,&t_opentime,MPI_MAX,_PARTEST_DOUBLE);
	  reduce_omp(&closetime,&t_closetime,MPI_MAX,_PARTEST_DOUBLE);
	  reduce_omp(&readtime,&t_readtime,MPI_MAX,_PARTEST_DOUBLE);
	#pragma omp master
	  {
		  MPI_Reduce(&t_bsumread, &sumsize, 1, SION_MPI_INT64, MPI_SUM, 0, communicators->workread);
		  MPI_Reduce(&t_opentime, &gopentime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->workread);
		  MPI_Reduce(&t_closetime, &gclosetime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->workread);
		  MPI_Reduce(&t_readtime, &greadtime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->workread);
	  }
#pragma omp barrier
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "after red.");
	#pragma omp master
	  {
		  if (communicators->workread_rank == 0) {
			fprintf(stderr, "------------------------------------------------------------------------------------------\n");
			fprintf(stderr, "TOTAL result: open=%10.6fs close=%10.6fs read %10.4f MB read=%10.6fs br=%10.4f MB/s from %d files\n",
					gopentime, gclosetime, 1.0 * sumsize / 1024.0 / 1024.0, greadtime, 1.0 * sumsize / 1024.0 / 1024.0 / greadtime, options->numfiles);
			fprintf(stderr, "------------------------------------------------------------------------------------------\n");
		  }
	  }
#pragma omp barrier
  
  if(options->unlink_files) {
    /*                                    */ starttime = MPI_Wtime();
    barrier_before_unlink(communicators->workread);
	#pragma omp master
	{
		if (communicators->local_rank == 0) {
		  fprintf(stderr, "partest result: unlink file %s ...\n", newfname);
		  unlink(newfname);
		}
	}
#pragma omp barrier
    barrier_after_unlink(communicators->workread);
    /*                                    */ unlinktime = MPI_Wtime() - starttime;
	#pragma omp master
    {
		if (communicators->local_rank == 0) {
		  fprintf(stderr, "partest result:  ultime=%10.6fs unlink %s\n", unlinktime, newfname);
		}
    }
#pragma omp barrier
  }

  return (1);
}

