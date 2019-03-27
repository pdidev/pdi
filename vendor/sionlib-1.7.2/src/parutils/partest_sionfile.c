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
#include "partest.h"
#include "partest_util.h"

sion_int64 partest_write_chunk_to_sionfile(  int            sid,
					     int            rank,
					     char          *localbuffer,
					     _test_options *options,
					     int            do_debug,
					     double        *checksum_fp,
					     int           *chunkcnt);

sion_int64 partest_read_chunk_from_sionfile(  int            sid,
					      int            rank,
					      char          *localbuffer,
					      _test_options *options,
					      int            do_debug,
					      double        *checksum_fp,
					      int           *chunkcnt);

  /****************************************************************************************************************
   *
   *                      test_paropen_multi_mpi
   *
   ***************************************************************************************************************/
int test_paropen_multi_mpi(char *filename,
			   char *localbuffer,
			   _test_communicators *communicators, 
			   _test_options *options
			   ) {

  double    starttime, write_starttime, read_starttime, unlinktime, gunlinktime;
  double    timings[TIMINGS_MAX_NUM],ftimings[TIMINGS_MAX_NUM],gtimings[TIMINGS_MAX_NUM];
  sion_int64  stats[STATS_MAX_NUM],  fstats[STATS_MAX_NUM],    gstats[STATS_MAX_NUM];

  double    checksum_fp, checksum_read_fp;
  int       globalrank, sid, i;
  int       chunkcnt, using_hints;
  sion_int64 rchunksize;
  sion_int32 rfsblksize;
  char      *newfname=NULL;
  int       do_debug;
  int       ser_count,ser_step,ser_done;
  char     *file_mode;

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "start");
  /* numfiles>=1 -> sion will split the communicator  */
  /* numfiles<=0 -> communicators->local contain correct local communicator, computed by split_communicator  */

  /* not working task ? */
  if (communicators->work_size == -1) {
    return (0);
  }

  if(0){

    MPI_Comm_size(communicators->work, &communicators->work_size);
    MPI_Comm_rank(communicators->work, &communicators->work_rank);
    MPI_Comm_size(communicators->workread, &communicators->workread_size);
    MPI_Comm_rank(communicators->workread, &communicators->workread_rank);
    fprintf(stderr, "timings[%06d] entering test_paropen_multi_mpi work=%d of %d workread=%d of %d\n", communicators->all_rank,
	    communicators->work_rank, communicators->work_size, 
	    communicators->workread_rank, communicators->workread_size 
	    );
  }


  do_debug=(((options->debug && communicators->all_rank == 0)) || ((options->Debug && (communicators->all_rank+1) == communicators->all_size)));

  for(i=0;i<TIMINGS_MAX_NUM;i++) timings[i]=ftimings[i]=gtimings[i]=0.0;
  for(i=0;i<STATS_MAX_NUM;i++)   stats[i]=fstats[i]=gstats[i]=0;

  if(options->do_write) {
    /****************************** WRITE *****************************/

    /* to synchronize start */
    barrier_after_start(communicators->local);

    /* TIMING                                   */ write_starttime = starttime = MPI_Wtime();
    if(options->use_posix) {
      file_mode="bw,posix";
    } else {
      file_mode="bw,ansi";
    }
    sid = sion_paropen_mpi(filename, file_mode, &options->numfiles, communicators->work, &communicators->local, 
			   &options->chunksize, &options->fsblksize, &globalrank, NULL, &newfname);
    using_hints=sion_using_hints(sid);
    stats[STATS_WR_NUM_FILES]=options->numfiles;
    /* TIMING                                   */ timings[TIMINGS_WR_OPEN] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_open(communicators->local);
    /* TIMING                                   */ timings[TIMINGS_WR_OPEN_BARR_FILE] += MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_open(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_WR_OPEN_BARR_GLOBAL] = MPI_Wtime()-starttime;

    /* local is computed again by sion_paropen_mpi if numfiles >=1 */
    /* TIMING                                   */ starttime = MPI_Wtime();
    MPI_Comm_size(communicators->local, &communicators->local_size);
    MPI_Comm_rank(communicators->local, &communicators->local_rank);
    
    if(options->serialize_blocknum==-2) sion_startof_transaction_mpi(sid);
    if(options->serialize_blocknum>0)   ser_step=options->serialize_blocknum;
    else                                ser_step=communicators->local_size;
    ser_done=0;
    /* TIMING                                   */ timings[TIMINGS_WR_WRITE_SYNC] += MPI_Wtime()-starttime;

    for(ser_count=0;ser_count<communicators->local_size;ser_count+=ser_step) {
      if ((!ser_done) && communicators->local_rank<(ser_count+ser_step)) {
	ser_done=1;

	/* TIMING                                   */ starttime = MPI_Wtime();
	stats[STATS_BYTES_WR_WROTE]+=partest_write_chunk_to_sionfile(sid,communicators->all_rank,
								     localbuffer,options,do_debug&&options->verbose,
								     &checksum_fp,&chunkcnt);
	stats[STATS_BYTES_WR_NUM_CHUNKS]+=chunkcnt;
	/* TIMING                                   */ timings[TIMINGS_WR_WRITE] += MPI_Wtime()-starttime;

      }

      /* TIMING                                   */ starttime = MPI_Wtime();
      if(options->serialize_blocknum==-2) sion_endof_transaction_mpi(sid);
      /* TIMING                                   */ timings[TIMINGS_WR_WRITE_SYNC] += MPI_Wtime()-starttime;

      /* TIMING                                   */ starttime = MPI_Wtime();
      barrier_after_write(communicators->local);
      /* TIMING                                   */ timings[TIMINGS_WR_WRITE_BARR_FILE] += MPI_Wtime()-starttime;
      
    }

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_write(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_WR_WRITE_BARR_GLOBAL] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    sion_parclose_mpi(sid);
    /* TIMING                                   */ timings[TIMINGS_WR_CLOSE] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_close(communicators->local);
    /* TIMING                                   */ timings[TIMINGS_WR_CLOSE_BARR_FILE] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_close(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_WR_CLOSE_BARR_GLOBAL] = MPI_Wtime()-starttime;
    /* TIMING                                   */ timings[TIMINGS_WR_TOTAL] = MPI_Wtime()-write_starttime;

    if (timings[TIMINGS_WR_TOTAL] == 0) timings[TIMINGS_WR_TOTAL] = -1;


    /* TIMING                                   */ starttime = MPI_Wtime();
    if ( (communicators->work_size>0) && (communicators->work_rank==0) ) {
      fprintf(stderr, "partest filespec:    (  )    using_hints           = %ld\n", (long) using_hints);
      fprintf(stderr, "partest filespec:    (  )    fsblksize             = %ld\n", (long) options->fsblksize);
    }

    if (options->verbose) {
      write_timings("TASK",TIMINGS_METHOD_WRITE,timings,stats,communicators,options,1);
    }
    if (do_debug) {
      write_timings("TASK",TIMINGS_METHOD_WRITE,timings,stats,communicators,options,0);
    }

    /* TIMING                                   */ timings[TIMINGS_MSGS] += MPI_Wtime()-starttime;

    if (options->numfiles > 1) {
      
      MPI_Reduce(timings, ftimings, TIMINGS_MAX_NUM, MPI_DOUBLE, MPI_MAX, 0, communicators->local);
      MPI_Reduce(stats, fstats, STATS_MAX_NUM, SION_MPI_INT64, MPI_SUM, 0, communicators->local);

      if (communicators->local_rank == 0) {
	write_timings("FILE",TIMINGS_METHOD_WRITE,ftimings,fstats,communicators,options,0);
      }

    }

    MPI_Reduce(timings, gtimings, TIMINGS_MAX_NUM, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
    MPI_Reduce(stats, gstats, STATS_MAX_NUM, SION_MPI_INT64, MPI_SUM, 0, communicators->work);

    if (communicators->work_rank == 0) {
      fprintf(stderr, "------------------------------------------------------------------------------------------\n");
      write_timings("TOTAL",TIMINGS_METHOD_WRITE,gtimings,gstats,communicators,options,0);
      fprintf(stderr, "------------------------------------------------------------------------------------------\n");
    }

    if(newfname) {free(newfname);newfname=NULL;}


  } /* do_write */

  /* to synchronize after write */
  barrier_after_close(communicators->work);

  if(options->do_read) {
    /****************************** READ *****************************/

    /* reset localbuffer */
    for (i = 0; i < ((options->totalsize < options->bufsize) ? options->totalsize : options->bufsize); i++) {
      localbuffer[i] = ' ';
    }
    /* to synchronize start */
    barrier_after_start(communicators->local);

    /* TIMING                                   */ read_starttime = starttime = MPI_Wtime();
    if(options->use_posix) {
      file_mode="br,posix";
    } else {
      file_mode="br,ansi";
    }

    /* TIMING                                   */ starttime = MPI_Wtime();
    if (options->collectiveopenforread) {
      /* commlocal and numfiles will be read from sion file */
      sid = sion_paropen_mpi(filename,file_mode, &options->numfiles, communicators->workread, 
			     &communicators->local, &options->chunksize, &options->fsblksize, &globalrank, NULL, &newfname);
      stats[STATS_RD_NUM_FILES]=options->numfiles;
    }
    else {
      /* there is some work to for multifile sion file */
      sid = sion_open_rank(filename, file_mode, &rchunksize, &rfsblksize, &communicators->workread_rank, NULL);
      stats[STATS_RD_NUM_FILES]=-1;
    }
    using_hints=sion_using_hints(sid);
    /* TIMING                                   */ timings[TIMINGS_RD_OPEN] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_open(communicators->local);
    /* TIMING                                   */ timings[TIMINGS_RD_OPEN_BARR_FILE] += MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_open(communicators->workread);
    /* TIMING                                   */ timings[TIMINGS_RD_OPEN_BARR_GLOBAL] = MPI_Wtime()-starttime;


    /* TIMING                                   */ starttime = MPI_Wtime();
    /* local is computed again by sion_paropen_mpi if numfiles >=1 */
    MPI_Comm_size(communicators->local, &communicators->local_size);
    MPI_Comm_rank(communicators->local, &communicators->local_rank);

    if(options->serialize_blocknum==-2) sion_startof_transaction_mpi(sid);
    if(options->serialize_blocknum>0)   ser_step=options->serialize_blocknum;
    else                                ser_step=communicators->local_size;
    ser_done=0;

    /* TIMING                                   */ timings[TIMINGS_RD_READ_SYNC] += MPI_Wtime()-starttime;

    for(ser_count=0;ser_count<communicators->local_size;ser_count+=ser_step) {
 
      if ((!ser_done) && communicators->local_rank<(ser_count+ser_step)) {
	ser_done=1;
      
	/* TIMING                                   */ starttime = MPI_Wtime();
	stats[STATS_BYTES_RD_READ]+=partest_read_chunk_from_sionfile(sid,communicators->all_rank,
								     localbuffer,options,do_debug&&options->verbose,
								     &checksum_read_fp,&chunkcnt);
	stats[STATS_BYTES_RD_NUM_CHUNKS]+=chunkcnt;
	/* TIMING                                   */ timings[TIMINGS_RD_READ] += MPI_Wtime()-starttime;
      
      }
      /* TIMING                                   */ starttime = MPI_Wtime();
      if(options->serialize_blocknum==-2) sion_endof_transaction_mpi(sid);
      /* TIMING                                   */ timings[TIMINGS_RD_READ_SYNC] += MPI_Wtime()-starttime;

      /* TIMING                                   */ starttime = MPI_Wtime();
      barrier_after_read(communicators->local); 
      /* TIMING                                   */ timings[TIMINGS_RD_READ_BARR_FILE] += MPI_Wtime()-starttime;

    }

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_read(communicators->workread);
    /* TIMING                                   */ timings[TIMINGS_RD_READ_BARR_GLOBAL] = MPI_Wtime()-starttime;


    /* TIMING                                   */ starttime = MPI_Wtime();
    if (options->collectiveopenforread) {
      sion_parclose_mpi(sid);
    }
    else {
      sion_close(sid);
    }
    /* TIMING                                   */ timings[TIMINGS_RD_CLOSE] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_close(communicators->local);
    /* TIMING                                   */ timings[TIMINGS_RD_CLOSE_BARR_FILE] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_close(communicators->workread);
    /* TIMING                                   */ timings[TIMINGS_RD_CLOSE_BARR_GLOBAL] = MPI_Wtime()-starttime;
    /* TIMING  */ timings[TIMINGS_RD_TOTAL] = MPI_Wtime()-read_starttime;


    /* TIMING                                   */ starttime = MPI_Wtime();
    if (timings[TIMINGS_RD_TOTAL] == 0) timings[TIMINGS_RD_TOTAL] = -1;

    if ( (communicators->work_size>0) && (communicators->workread_rank==0) ) {
      fprintf(stderr, "partest filespec:    (  )    using_hints           = %ld\n", (long) using_hints);
      fprintf(stderr, "partest filespec:    (  )    fsblksize             = %ld\n", (long) options->fsblksize);
    }

    if (options->verbose) {
      write_timings("TASK",TIMINGS_METHOD_READ,timings,stats,communicators,options,1);
    }

    if (do_debug) {
      write_timings("TASK",TIMINGS_METHOD_READ,timings,stats,communicators,options,0);
    }

    /* TIMING                                   */ timings[TIMINGS_MSGS] += MPI_Wtime()-starttime;

    if (options->numfiles > 1) {
      
      MPI_Reduce(timings, ftimings, TIMINGS_MAX_NUM, MPI_DOUBLE, MPI_MAX, 0, communicators->local);
      MPI_Reduce(stats, fstats, STATS_MAX_NUM, SION_MPI_INT64, MPI_SUM, 0, communicators->local);

      if (communicators->local_rank == 0) {
	write_timings("FILE",TIMINGS_METHOD_READ,ftimings,fstats,communicators,options,0);
      }

    }

    MPI_Reduce(timings, gtimings, TIMINGS_MAX_NUM, MPI_DOUBLE, MPI_MAX, 0, communicators->workread);
    MPI_Reduce(stats, gstats, STATS_MAX_NUM, SION_MPI_INT64, MPI_SUM, 0, communicators->workread);

    /*                                                                      */ DPRINTFTS(communicators->all_rank, "after red.");
    if (communicators->workread_rank == 0) {
      fprintf(stderr, "------------------------------------------------------------------------------------------\n");
      write_timings("TOTAL",TIMINGS_METHOD_READ,gtimings,gstats,communicators,options,0);
      fprintf(stderr, "------------------------------------------------------------------------------------------\n");
    }

#ifdef CHECKSUM
    if(!options->suppress_checksum) {
      if (fabs(checksum_fp - checksum_read_fp) > 1e-5) {
	fprintf(stderr, "timings[%06d] ERROR in double checksum  %14.10f==%14.10f, diff=%14.10f\n", communicators->local_rank,
		checksum_fp, checksum_read_fp, checksum_fp - checksum_read_fp);
      }
    }
#endif

  } /* do_read */

  if(options->unlink_files) {
    /*                                    */ starttime = MPI_Wtime();
    barrier_before_unlink(communicators->workread);
    if (communicators->local_rank == 0) {
      fprintf(stderr, "partest result: unlink file %s ...\n", newfname);
      unlink(newfname);
    }
    barrier_after_unlink(communicators->workread);
    /*                                    */ unlinktime = MPI_Wtime() - starttime;
    MPI_Reduce(&unlinktime, &gunlinktime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
    if (communicators->work_rank == 0) {
      fprintf(stderr, "partest result:  ultime=%10.6fs unlink %s\n", gunlinktime, newfname);
    }
  }

  if(newfname) {free(newfname);newfname=NULL;}

  return (1);
}

sion_int64 partest_write_chunk_to_sionfile(  int            sid,
					     int            rank,
					     char          *localbuffer,
					     _test_options *options,
					     int            do_debug,
					     double        *checksum_fp,
					     int           *chunkcnt) {
  sion_int64 left;
  sion_int64 bsumwrote;
  size_t    bytes_in_chunk; 
  size_t    bwrite, bwrote;
  int       i;

  sion_int64   bufsize     = (sion_int64) options->bufsize;
  sion_int64   totalsize   = (sion_int64) options->totalsize;
  sion_int64   startoffset = options->startoffset;

  *checksum_fp=0.0;
  left = totalsize;
  bsumwrote = 0;
  *chunkcnt = 0;
  bytes_in_chunk=0;

  /* Write until  total size of the data is reached */
  while (left > 0) {
    if(startoffset==0) bwrite = bufsize;
    else {
      bwrite = startoffset; startoffset=0;
    }
    if (bwrite > left)
      bwrite = left;
	
    if (0) {
      fprintf(stderr, "timings[%06d] write %lld bytes\n", rank, (sion_int64) bwrite);
    }
	
    bytes_in_chunk+=bwrite;
    if(bytes_in_chunk>options->chunksize) {
      sion_ensure_free_space(sid, bwrite);
      bytes_in_chunk=bwrite;
    }
    if(options->collectivewrite) {
      bwrote = sion_coll_fwrite_mpi(localbuffer, 1, bwrite, sid);
    } else {
      bwrote = sion_fwrite(localbuffer, 1, bwrite, sid);
    }
	
#ifdef CHECKSUM
    if(!options->suppress_checksum) {
      for (i = 0; i < bwrote; i++)
	*checksum_fp += (double) localbuffer[i];
    }
#endif
    left -= bwrote;
    bsumwrote += bwrote;
    (*chunkcnt)++;
	
    if (do_debug) {
      fprintf(stderr, "timings[%06d] wrote %10lld bytes total: wrote %14lld bytes (%10.4f MB) left %14lld bytes (%10.4f MB)\n", rank, 
	      (sion_int64) bwrote,
	      bsumwrote, bsumwrote / 1024.0 / 1024.0, 
	      (sion_int64) left,
	      left  / 1024.0 / 1024.0 );
    }
  }
  return(bsumwrote);
}


sion_int64 partest_read_chunk_from_sionfile(  int            sid,
					      int            rank,
					      char          *localbuffer,
					      _test_options *options,
					      int            do_debug,
					      double        *checksum_read_fp,
					      int           *chunkcnt) {

  sion_int64 left;
  sion_int64 bsumread;
  size_t     bytes_in_chunk; 
  size_t     btoread, bread;
  int        myfeof;
  int        i;

  sion_int64   bufsize     = (sion_int64) options->bufsize;
  sion_int64   totalsize   = (sion_int64) options->totalsize;
  sion_int64   startoffset = options->startoffset;
  
  *checksum_read_fp = 0;
  left              = totalsize;
  bsumread = 0;
  *chunkcnt = 0;
  bytes_in_chunk = 0;

      
  myfeof=sion_feof(sid);
  while ((left > 0) && (!myfeof)) {
	
    if(startoffset==0) btoread = bufsize;
    else {
      btoread = startoffset; startoffset=0;
    }
    if (btoread > left)
      btoread = left;
      
    bytes_in_chunk+=btoread;
    if(bytes_in_chunk>options->chunksize) {
      myfeof=sion_feof(sid);
    }
    if(!myfeof) {
    if(options->collectiveread) {
      bread  = sion_coll_fread_mpi(localbuffer, 1, btoread, sid);
    } else {
      bread = sion_fread(localbuffer, 1, btoread, sid);
    }
      
#ifdef CHECKSUM
      if(!options->suppress_checksum) {
	for (i = 0; i < bread; i++)
	  *checksum_read_fp += (double) localbuffer[i];
      }
#endif
      
      left -= bread;
      bsumread += bread;
      (*chunkcnt)++;

      if (do_debug) {
	fprintf(stderr, "timings[%06d] read %10lld bytes total: read %14lld bytes (%10.4f MB) left %14lld bytes (%10.4f MB)\n", rank, 
		(sion_int64) bread,
		bsumread, bsumread / 1024.0 / 1024.0, 
		(sion_int64) left,
		left  / 1024.0 / 1024.0 );
      }
    }
  }

  return(bsumread);
}


