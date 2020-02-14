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
#include "sion_debug.h"
#include "sion_printts.h"
#include "partest.h"
#include "partest_util.h"

int test_mpiio_multi_mpi(char *filename,
                         char *localbuffer,
			 _test_communicators *communicators, 
			 _test_options *options
			 ) {

  char     *ptr;
  int       err;
  int       i, lg;
  char      file[FNAMELEN];

  double    gstarttime, starttime, gopentime, opentime, unlinktime, gwritetime, writetime, gclosetime, closetime, readtime, greadtime;
  double    barr1time, barr2time, barr3time;

  sion_int64 left;
  sion_int64 bsumwrote, sumsize, bsumread;
  char      cbuffer[MAXCHARLEN];
  int       bwrite, bwrote, btoread, bread, chunkcnt = 0;
  double    checksum_fp = 0, checksum_read_fp = 0;

  int       ioflags = 0;
  char      msg[2048];
  char      option[1024];
  MPI_File  fh, fhin;
  MPI_Status status;
  MPI_Offset offset;
  MPI_Info  info = MPI_INFO_NULL;
  int       info_created = 0;
  int       ser_count,ser_step,ser_done;


  /* not working task ? */
  if (communicators->work_size == -1) {
    return (0);
  }

  if (options->mpiio_lb >= 0) {
    /*
     * This hint can be used only if all tasks are being used for I/O:
     * either the MP_IONODEFILE environment variable is not set,
     * or it specifies a file that lists all nodes on which the application is running.
     */
    if (info_created == 0) {
      MPI_Info_create(&info);
      info_created = 1;
    }

    if (options->mpiio_lb == 0) {
      if (communicators->work_rank == 0)
        fprintf(stderr, "partest[%03d] set IBM_largeblock_io to false\n", communicators->work_rank);
      MPI_Info_set(info, "IBM_largeblock_io", "false");
    }
    else {
      if (communicators->work_rank == 0)
        fprintf(stderr, "partest[%03d] set IBM_largeblock_io to true\n", communicators->work_rank);
      MPI_Info_set(info, "IBM_largeblock_io", "true");
    }
  }

  if (options->mpiio_bs >= 0) {

    if (info_created == 0) {
      MPI_Info_create(&info);
      info_created = 1;
    }
    sprintf(option, "%dKB", options->mpiio_bs);
    if (communicators->work_rank == 0)
      fprintf(stderr, "partest[%03d] set IBM_io_buffer_size to %s\n", communicators->work_rank, option);
    MPI_Info_set(info, "IBM_io_buffer_size", option);
  }

  if (options->mpiio_sa >= 0) {
    if (info_created == 0) {
      MPI_Info_create(&info);
      info_created = 1;
    }
    if (options->mpiio_sa == 0) {
      if (communicators->work_rank == 0)
        fprintf(stderr, "partest[%03d] set IBM_sparse_access to false\n", communicators->work_rank);
      MPI_Info_set(info, "IBM_sparse_access", "false");
    }
    else {
      if (communicators->work_rank == 0)
        fprintf(stderr, "partest[%03d] set IBM_sparse_access to true\n", communicators->work_rank);
      MPI_Info_set(info, "IBM_sparse_access", "true");
    }
  }

  sprintf(file, "%s.%06d", filename, communicators->file_number);

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "start");

  /****************************** WRITE *****************************/

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[W]before open");
  /*                                    */ starttime = MPI_Wtime();
  ioflags = MPI_MODE_CREATE | MPI_MODE_WRONLY;
  if ((err = MPI_File_open(communicators->local, file, ioflags, info, &fh)) != MPI_SUCCESS) {
    lg = sizeof(msg);
    MPI_Error_string(err, msg, &lg);
    fprintf(stderr, "[%04d]: MPI_File5A_open [%s]: impossible to open %s\n", communicators->all_rank, msg, file);
    MPI_Abort(communicators->all, 1);
    exit(1);
  }
  /*                                    */ opentime = MPI_Wtime() - starttime;

  offset = communicators->local_rank * options->totalsize;

  /*
    MPI_Datatype buffer;
    MPI_Type_contiguous(bufsize,MPI_CHAR,&buffer);
    MPI_Type_commit(&buffer);
    MPI_File_set_view(fh, offset,MPI_CHAR, buffer, "native", MPI_INFO_NULL);
  */

  /*                                    */ starttime = MPI_Wtime();
  barrier_after_open(communicators->work);
  /*                                    */ barr1time = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[W]after open");
  /*                                    */ gstarttime = starttime = MPI_Wtime();

  if(options->serialize_blocknum>0) ser_step=options->serialize_blocknum;
  else                              ser_step=communicators->local_size;
  ser_done=0;
  for(ser_count=0;ser_count<communicators->local_size;ser_count+=ser_step) {
    if ((!ser_done) && communicators->local_rank<(ser_count+ser_step)) {
      /* fprintf(stderr, "starting write on task %d ser_count=%d ser_step=%d ser_done=%d\n", communicators->local_rank,ser_count,ser_step,ser_done); */
      ser_done=1;

      /* here we hope each node has the same memory size ... */
      if ((err = MPI_File_seek(fh, offset, MPI_SEEK_SET)) != MPI_SUCCESS) {
	lg = sizeof(msg);
	MPI_Error_string(err, msg, &lg);
	fprintf(stderr, "[%04d]: MPI_File_seek [%s] : cannot seek in %s\n", communicators->all_rank, msg, file);
	MPI_Abort(communicators->all, 1);
	exit(1);
      }

      ptr = localbuffer;
      left = options->totalsize;
      bsumwrote = 0;
      chunkcnt = 0;
      bwrote = 0;

      while (left > 0) {
	bwrite = (int) options->bufsize;
	if (bwrite > left)
	  bwrite = (int) left;

	if (((options->debug && communicators->all_rank == 0)) || ((options->Debug && communicators->all_rank == communicators->all_size))) {
	  fprintf(stderr, "timings[%03d] write %lld bytes\n", communicators->all_rank, (sion_int64) bwrite);
	}

	if ((err = MPI_File_write(fh, ptr, bwrite, MPI_CHAR, &status)) != MPI_SUCCESS) {
	  lg = sizeof(msg);
	  MPI_Error_string(err, msg, &lg);
	  fprintf(stderr, "[%04d]: MPI_File_write [%s] : impossible to write in %s\n", communicators->all_rank, msg, file);
	  MPI_Abort(communicators->all, 1);
	  exit(1);
	}

	MPI_Get_count(&status, MPI_CHAR, &bwrote);
	left -= (sion_int64) bwrote;
	bsumwrote += (sion_int64) bwrote;
	chunkcnt++;

#ifdef CHECKSUM
	checksum_fp=0.0;
	for (i = 0; i < bwrote; i++)
	  checksum_fp += (double) localbuffer[i];
#endif

	if (((options->debug && communicators->all_rank == 0)) || ((options->Debug && communicators->all_rank == communicators->all_size))) {
	  fprintf(stderr, "timings[%03d] wrote (%lld bytes) %lld bytes (%10.4f MB) (%lld left)\n",
		  communicators->all_rank, (sion_int64) bwrote, bsumwrote, bsumwrote / 1024.0 / 1024.0, (sion_int64) left);
	  MPI_File_get_position(fh, &offset);
	  fprintf(stderr, "timings[%03d] after write   position in file= %lld \n", communicators->all_rank, offset);

	}
      }
    }
    barrier_after_write(communicators->local);
    /* fprintf(stderr, "after barrier on local comm on task %d ser_count=%d ser_step=%d ser_done=%d\n", communicators->local_rank,ser_count,ser_step,ser_done); */
    /* barrier_after_write(communicators->local); */
  }

  /*                                    */ writetime = MPI_Wtime() - starttime;
  /*                                    */ starttime = MPI_Wtime();
  barrier_after_write(communicators->work);
  /*                                    */ barr2time = MPI_Wtime() - starttime;
  /*                                    */ gwritetime = MPI_Wtime() - gstarttime;
  /*                                    */ starttime = MPI_Wtime();
  MPI_File_close(&fh);
  /*                                    */ closetime = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[W]before close");
  /*                                    */ starttime = MPI_Wtime();
  barrier_after_close(communicators->work);
  /*                                    */ barr3time = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[W]after close");

  if (options->verbose) {
    sprintf(cbuffer,
            "timings[%03d] open=%10.6fs write=%10.6fs close=%10.6fs barrier(open=%10.6fs, write=%10.6fs, close=%10.6fs) #chunks=%d bw=%10.4f MB/s ionode=%d\n",
            communicators->all_rank, opentime, writetime, closetime, barr1time, barr2time, barr3time, chunkcnt,
            options->totalsize / 1024.0 / 1024.0 / writetime, communicators->ionode_number);
    collective_print_gather(cbuffer, communicators->work);
  }

  if (options->numfiles >= 1) {
    MPI_Reduce(&bsumwrote, &sumsize, 1, SION_MPI_INT64, MPI_SUM, 0, communicators->local);
    if (communicators->local_rank == 0) {
      fprintf(stderr, "partest result: wrote %10.4f MB to %s\n", 1.0 * sumsize / 1024.0 / 1024.0, file);
    }
  }

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "before red.");
  MPI_Reduce(&bsumwrote, &sumsize, 1, SION_MPI_INT64, MPI_SUM, 0, communicators->work);
  MPI_Reduce(&opentime, &gopentime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
  MPI_Reduce(&closetime, &gclosetime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "after red.");
  if (communicators->work_rank == 0) {
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
    fprintf(stderr, "TOTAL result: open=%10.6fs close=%10.6fs wrote %10.4f MB write+barrier=%10.6fs bw=%10.4f MB/s to %d files\n",
            gopentime, gclosetime, 1.0 * sumsize / 1024.0 / 1024.0, gwritetime, 1.0 * sumsize / 1024.0 / 1024.0 / gwritetime, options->numfiles);
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
  }

  if (communicators->work_rank == 0)
    fprintf(stderr, "*********************************************************************************************\n");

  /****************************** READ *****************************/

  /*                                    */ DPRINTFTS(communicators->all_rank, "[R]before open");
  /*                                    */ starttime = MPI_Wtime();


  ioflags = MPI_MODE_RDONLY;

  if ((err = MPI_File_open(communicators->local, file, ioflags, info, &fhin)) != MPI_SUCCESS) {
    lg = sizeof(msg);
    MPI_Error_string(err, msg, &lg);
    fprintf(stderr, "[%04d]: MPI_File_open [%s]: impossible to open %s\n", communicators->all_rank, msg, file);
    MPI_Abort(communicators->all, 1);
    exit(1);
  }
  /*                                    */ opentime = MPI_Wtime() - starttime;

  /*                                    */ starttime = MPI_Wtime();
  barrier_after_open(communicators->work);
  /*                                    */ barr1time = MPI_Wtime() - starttime;
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "[R]after open");
  /*                                    */ gstarttime = starttime = MPI_Wtime();

  if(options->serialize_blocknum>0) ser_step=options->serialize_blocknum;
  else                              ser_step=communicators->local_size;
  ser_done=0;
  for(ser_count=0;ser_count<communicators->local_size;ser_count+=ser_step) {
 
    if ((!ser_done) && communicators->local_rank<(ser_count+ser_step)) {
      /* fprintf(stderr, "starting read on task %d ser_count=%d ser_step=%d ser_done=%d\n", communicators->local_rank,ser_count,ser_step,ser_done); */
      ser_done=1;

      /* here we hope each node has the same memory size ... */
      offset = communicators->local_rank * options->totalsize;
      if ((err = MPI_File_seek(fhin, offset, MPI_SEEK_SET)) != MPI_SUCCESS) {
	lg = sizeof(msg);
	MPI_Error_string(err, msg, &lg);
	fprintf(stderr, "[%04d]: MPI_File_seek [%s] : cannot seek in %s\n", communicators->all_rank, msg, file);
	MPI_Abort(communicators->all, 1);
	exit(1);
      }

      ptr = localbuffer;
      left = options->totalsize;
      bsumread = 0;
      chunkcnt = 0;

      while (left > 0) {
	btoread = options->bufsize;
	if (btoread > left)
	  btoread = left;

	if ((err = MPI_File_read(fhin, ptr, btoread, MPI_CHAR, &status)) != MPI_SUCCESS) {
	  lg = sizeof(msg);
	  MPI_Error_string(err, msg, &lg);
	  fprintf(stderr, "[%04d]: MPI_File_read [%s]: impossible to read in %s\n", communicators->all_rank, msg, file);
	  MPI_Abort(communicators->all, 1);
	  exit(1);
	}

	MPI_Get_count(&status, MPI_CHAR, &bread);
	left -= bread;
	bsumread += bread;
	chunkcnt++;

#ifdef CHECKSUM
	checksum_read_fp=0.0;
	for (i = 0; i < bread; i++)
	  checksum_read_fp += (double) localbuffer[i];
#endif

	if (((options->debug && communicators->all_rank == 0)) || ((options->Debug && communicators->all_rank == communicators->all_size))) {
	  fprintf(stderr, "timings[%03d] read (%lld bytes) %lld bytes (%10.4f MB) (%lld left)\n", communicators->all_rank, (sion_int64) bread, bsumread,
		  bsumread / 1024.0 / 1024.0, (sion_int64) left);
	}
      }

    }
    barrier_after_read(communicators->local); 
    /* fprintf(stderr, "after barrier on local comm on task %d ser_count=%d ser_step=%d ser_done=%d\n", communicators->local_rank,ser_count,ser_step,ser_done); */
    /* barrier_after_read(communicators->local); */
  }

  /*                                    */ readtime = MPI_Wtime() - starttime;
  /*                                    */ starttime = MPI_Wtime();
  barrier_after_read(communicators->work);
  /*                                    */ barr2time = MPI_Wtime() - starttime;
  /*                                    */ greadtime = MPI_Wtime() - gstarttime;

  /*                                    */ starttime = MPI_Wtime();
  MPI_File_close(&fhin);
  /*                                    */ closetime = MPI_Wtime() - starttime;
  barrier_after_close(communicators->work);

  if (options->verbose) {
    sprintf(cbuffer,
            "timings[%03d] open=%10.6fs read=%10.6fs close=%10.6fs barrier(open=%10.6fs, read=%10.6fs, close=%10.6fs) #chunks=%d bw=%10.4f MB/s ionode=%d (check %d)\n",
            communicators->all_rank, opentime, readtime, closetime, barr1time, barr2time, barr3time, chunkcnt,
            options->totalsize / 1024.0 / 1024.0 / readtime, communicators->ionode_number, (fabs(checksum_fp - checksum_read_fp) < 1e-5));

    collective_print_gather(cbuffer, communicators->work);

  }

#ifdef CHECKSUM
  if (fabs(checksum_fp - checksum_read_fp) > 1e-5) {
    fprintf(stderr, "timings[%03d] ERROR in double checksum  %14.10f==%14.10f, diff=%14.10f\n", communicators->all_rank,
            checksum_fp, checksum_read_fp, checksum_fp - checksum_read_fp);
  }
#endif

  if (options->numfiles >= 1) {
    MPI_Reduce(&bsumread, &sumsize, 1, SION_MPI_INT64, MPI_SUM, 0, communicators->local);
    if (communicators->local_rank == 0) {
      fprintf(stderr, "partest result: read  %10.4f MB from %s\n", 1.0 * sumsize / 1024.0 / 1024.0, file);
    }
  }

  /*                                                                      */ DPRINTFTS(communicators->all_rank, "before red.");
  MPI_Reduce(&bsumread, &sumsize, 1, SION_MPI_INT64, MPI_SUM, 0, communicators->work);
  MPI_Reduce(&opentime, &gopentime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
  MPI_Reduce(&closetime, &gclosetime, 1, MPI_DOUBLE, MPI_MAX, 0, communicators->work);
  /*                                                                      */ DPRINTFTS(communicators->all_rank, "after red.");
  if (communicators->work_rank == 0) {
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
    fprintf(stderr, "TOTAL result: open=%10.6fs close=%10.6fs read %10.4f MB read+barrier=%10.6fs br=%10.4f MB/s from %d files\n",
            gopentime, gclosetime, 1.0 * sumsize / 1024.0 / 1024.0, greadtime, 1.0 * sumsize / 1024.0 / 1024.0 / greadtime, options->numfiles);
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
  }


  if(options->unlink_files) {
    /*                                    */ starttime = MPI_Wtime();
    barrier_before_unlink(communicators->work);
    if (communicators->local_rank == 0) {
      fprintf(stderr, "partest result: unlink file %s ...\n", file);
      unlink(file);
    }
    barrier_after_unlink(communicators->work);
    /*                                    */ unlinktime = MPI_Wtime() - starttime;
    if (communicators->local_rank == 0) {
      fprintf(stderr, "partest result:  ultime=%10.6fs unlink %s\n", unlinktime, file);
    }
  }

  if (info_created)
    MPI_Info_free(&info);

  return (1);
}
