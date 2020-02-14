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
#include <unistd.h>
#include <mpi.h>
#include <time.h>
#include <math.h>

#include "sion.h"
#include "sion_printts.h"
#include "sion_debug.h"
#include "sion_file.h"
#include "partest.h"
#include "partest_util.h"

int
test_single_mpi(char *filename,
                char *localbuffer,
		_test_communicators *communicators, 
		_test_options *options) {

  int       i;
  char      fname[FNAMELEN];
  _sion_fileptr  *sion_fileptr; 

  double    starttime, write_starttime, read_starttime, unlinktime, gunlinktime;
  double    timings[TIMINGS_MAX_NUM],ftimings[TIMINGS_MAX_NUM],gtimings[TIMINGS_MAX_NUM];
  sion_int64  stats[STATS_MAX_NUM],  fstats[STATS_MAX_NUM],    gstats[STATS_MAX_NUM];

  sion_int64 left;
  sion_int64 bsumwrote, bsumread;
  size_t    bwrite, bwrote, btoread, bread, chunkcnt;
  double    checksum_fp, checksum_read_fp;
  int       ser_count,ser_step,ser_done;
  int       api_to_use;
  int       do_debug;

  /* not working task ? */
  if (communicators->work_size == -1) {
    /*     printf("wf: not working %d\n",communicators->all_rank); */
    return (0);
  }

  if(options->use_posix) {
    api_to_use=SION_FILE_FLAG_POSIX;
  } else {
    api_to_use=SION_FILE_FLAG_ANSI;
  }

  for(i=0;i<TIMINGS_MAX_NUM;i++) timings[i]=ftimings[i]=gtimings[i]=0.0;
  for(i=0;i<STATS_MAX_NUM;i++)   stats[i]=fstats[i]=gstats[i]=0;

  do_debug=(((options->debug && communicators->all_rank == 0)) || ((options->Debug && (communicators->all_rank+1) == communicators->all_size)));

  sprintf(fname, "%s.%06d", filename, communicators->work_rank);

  if(options->do_write) {

    /****************************** WRITE *****************************/

    /* to synchronize start */
    barrier_after_start(communicators->local);
    
    /* TIMING                                   */ write_starttime = starttime = MPI_Wtime();

    /* FIRST OPEN to create files */
    sion_fileptr = _sion_file_open(fname,api_to_use|SION_FILE_FLAG_WRITE|SION_FILE_FLAG_CREATE,0);
    if (!sion_fileptr) {
      fprintf(stderr, "cannot open %s for writing, aborting ...\n", fname);
      MPI_Abort(communicators->all, 1);
    }
    stats[STATS_WR_NUM_FILES]=communicators->work_size;
    /* TIMING                                   */ timings[TIMINGS_WR_CREATE] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_open(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_WR_CREATE_BARR_OPEN] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    _sion_file_close(sion_fileptr);
    /* TIMING                                   */ timings[TIMINGS_WR_CREATE_CLOSE] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_open(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_WR_CREATE_BARR_CLOSE] = MPI_Wtime()-starttime;


    /* TIMING                                   */ starttime = MPI_Wtime();
    /* SECOND REOPEN of existing files */
    sion_fileptr = _sion_file_open(fname,api_to_use|SION_FILE_FLAG_WRITE,0);
    if (!sion_fileptr) {
      fprintf(stderr, "cannot open %s for writing, aborting ...\n", fname);
      MPI_Abort(communicators->all, 1);
    }
    /* TIMING                                   */ timings[TIMINGS_WR_OPEN] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_open(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_WR_OPEN_BARR_GLOBAL] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    if(options->serialize_blocknum>0) ser_step=options->serialize_blocknum;
    else                              ser_step=communicators->local_size;
    ser_done=0;
    /* TIMING                                   */ timings[TIMINGS_WR_WRITE_SYNC] += MPI_Wtime()-starttime;
    
    
    for(ser_count=0;ser_count<communicators->local_size;ser_count+=ser_step) {

      if ((!ser_done) && communicators->local_rank<(ser_count+ser_step)) {
	ser_done=1;

	/* TIMING                                   */ starttime = MPI_Wtime();
	left = options->totalsize;
	bsumwrote = 0;
	chunkcnt = 0;

	while (left > 0) {
	  bwrite = options->bufsize;
	  if (bwrite > left)
	    bwrite = left;

	  if (do_debug) {
	    fprintf(stderr, "timings[%03d] write %lld bytes\n", communicators->all_rank, (sion_int64) bwrite);
	  }

	  bwrote = _sion_file_write(localbuffer, bwrite, sion_fileptr);
	
	  left -= (sion_int64) bwrote;
	  bsumwrote += (sion_int64) bwrote;
	  chunkcnt++;

#ifdef CHECKSUM
	  if(!options->suppress_checksum) {
	    checksum_fp=0.0;
	    for (i = 0; i < bwrote; i++)
	      checksum_fp += (double) localbuffer[i];
	  }
#endif

	  if (do_debug) {
	    fprintf(stderr, "timings[%03d] wrote (%lld bytes) %lld bytes (%10.4f MB) (%lld left)\n",
		    communicators->all_rank, (sion_int64) bwrote, bsumwrote, bsumwrote / 1024.0 / 1024.0, (sion_int64) left);
	  }

	} /* while (left>0) */

	/* TIMING                                   */ timings[TIMINGS_WR_WRITE] += MPI_Wtime()-starttime;
	stats[STATS_BYTES_WR_WROTE]+=bsumwrote;
	stats[STATS_BYTES_WR_NUM_CHUNKS]+=chunkcnt;
	
      }	/* ser */

      /* TIMING                                   */ starttime = MPI_Wtime();
      barrier_after_write(communicators->local);
      /* TIMING                                   */ timings[TIMINGS_WR_WRITE_BARR_FILE] += MPI_Wtime()-starttime;

    } /* for(ser ...) */

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_write(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_WR_WRITE_BARR_GLOBAL] = MPI_Wtime()-starttime;
    
    /* TIMING                                   */ starttime = MPI_Wtime();
    _sion_file_close(sion_fileptr);
    /* TIMING                                   */ timings[TIMINGS_WR_CLOSE] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_close(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_WR_CLOSE_BARR_GLOBAL] = MPI_Wtime()-starttime;

    /* TIMING                                   */ timings[TIMINGS_WR_TOTAL] = MPI_Wtime()-write_starttime;


    if (options->verbose) {
      write_timings("TASK",TIMINGS_METHOD_WRITE,timings,stats,communicators,options,1);
    }

    if (do_debug) {
      write_timings("TASK",TIMINGS_METHOD_WRITE,timings,stats,communicators,options,0);
    }

    MPI_Reduce(timings, gtimings, TIMINGS_MAX_NUM, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
    MPI_Reduce(stats, gstats, STATS_MAX_NUM, SION_MPI_INT64, MPI_SUM, 0, communicators->work);

    if (communicators->work_rank == 0) {
      fprintf(stderr, "------------------------------------------------------------------------------------------\n");
      write_timings("TOTAL",TIMINGS_METHOD_WRITE,gtimings,gstats,communicators,options,0);
      fprintf(stderr, "------------------------------------------------------------------------------------------\n");
    }

  } /* do_write */

  /* to synchronize after write */
  barrier_after_close(communicators->work);

  if(options->do_read) {
    /****************************** READ *****************************/

    sprintf(fname, "%s.%06d", filename, communicators->workread_rank);
    /*   printf("on rank %3d READ: filename %s\n",communicators->all_rank,fname);  */


    /* to synchronize start */
    barrier_after_start(communicators->local);

    /* TIMING                                   */ read_starttime = starttime = MPI_Wtime();

    sion_fileptr = _sion_file_open(fname,api_to_use|SION_FILE_FLAG_READ,0);
    if (!sion_fileptr) {
      fprintf(stderr, "cannot open %s for writing, aborting ...\n", fname);
      MPI_Abort(communicators->all, 1);
    }
    stats[STATS_RD_NUM_FILES]=communicators->work_size;
    /* TIMING                                   */ timings[TIMINGS_RD_OPEN] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_open(communicators->work);
    /* TIMING                                   */ timings[TIMINGS_RD_OPEN_BARR_GLOBAL] = MPI_Wtime()-starttime;
 

    /* TIMING                                   */ starttime = MPI_Wtime();
    if(options->serialize_blocknum>0) ser_step=options->serialize_blocknum;
    else                              ser_step=communicators->local_size;
    ser_done=0;
    /* TIMING                                   */ timings[TIMINGS_RD_READ_SYNC] += MPI_Wtime()-starttime;

    for(ser_count=0;ser_count<communicators->local_size;ser_count+=ser_step) {
 
      if ((!ser_done) && communicators->local_rank<(ser_count+ser_step)) {
	ser_done=1;

	/* TIMING                                   */ starttime = MPI_Wtime();
	left = options->totalsize;
	bsumread = 0;
	chunkcnt = 0;

	while (left > 0) {
	  btoread = options->bufsize;
	  if (btoread > left)
	    btoread = left;

	  bread = _sion_file_read(localbuffer, btoread, sion_fileptr);
	  
	  left -= bread;
	  bsumread += bread;
	  chunkcnt++;

#ifdef CHECKSUM
	  if(!options->suppress_checksum) {
	    checksum_read_fp=0.0;
	    for (i = 0; i < bread; i++)
	      checksum_read_fp += (double) localbuffer[i];
	  }
#endif

	  if (do_debug) {
	    fprintf(stderr, "timings[%03d] read (%lld bytes) %lld bytes (%10.4f MB) (%lld left)\n", communicators->all_rank, (sion_int64) bread, bsumread,
		    bsumread / 1024.0 / 1024.0, (sion_int64) left);
	  }

	} /* while(left>0) */

	/* TIMING                                   */ timings[TIMINGS_RD_READ] += MPI_Wtime()-starttime;

	stats[STATS_BYTES_RD_READ]+=bsumread;
	stats[STATS_BYTES_RD_NUM_CHUNKS]+=chunkcnt;

	/* TIMING                                   */ starttime = MPI_Wtime();
	barrier_after_read(communicators->local); 
	/* TIMING                                   */ timings[TIMINGS_RD_READ_BARR_FILE] += MPI_Wtime()-starttime;

      } /* if (ser...) */

    } /*  for (ser ...) */

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_read(communicators->workread);
    /* TIMING                                   */ timings[TIMINGS_RD_READ_BARR_GLOBAL] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    _sion_file_close(sion_fileptr);
    /* TIMING                                   */ timings[TIMINGS_RD_CLOSE] = MPI_Wtime()-starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();
    barrier_after_close(communicators->workread);
    /* TIMING                                   */ timings[TIMINGS_RD_CLOSE_BARR_GLOBAL] = MPI_Wtime()-starttime;
    /* TIMING  */ timings[TIMINGS_RD_TOTAL] = MPI_Wtime()-read_starttime;

    /* TIMING                                   */ starttime = MPI_Wtime();

    if (timings[TIMINGS_RD_TOTAL] == 0) timings[TIMINGS_RD_TOTAL] = -1;

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
	fprintf(stderr, "timings[%03d] ERROR in double checksum  %14.10f==%14.10f, diff=%14.10f\n", communicators->all_rank,
		checksum_fp, checksum_read_fp, checksum_fp - checksum_read_fp);
      }
    }
#endif

  }

  if(options->unlink_files) {
    /*                                    */ starttime = MPI_Wtime();
    barrier_before_unlink(communicators->work);
    unlink(fname);
    barrier_after_unlink(communicators->work);
    /*                                    */ unlinktime = MPI_Wtime() - starttime;
    MPI_Reduce(&unlinktime, &gunlinktime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
    if (communicators->work_rank == 0) {
      fprintf(stderr, "partest result:  ultime=%10.6fs unlink %s\n", gunlinktime, fname);
    }
  }

  return (1);
}
