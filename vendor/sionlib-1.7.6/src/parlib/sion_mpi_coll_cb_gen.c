/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

/*!
 * \file
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "mpi.h"

#define USE_PMPIno
#ifdef USE_PMPI
#define MPI_Comm_rank      PMPI_Comm_rank
#define MPI_Comm_size      PMPI_Comm_size	  
#define MPI_Gather	   PMPI_Gather	  
#define MPI_Scatter	   PMPI_Scatter	  
#define MPI_Bcast	   PMPI_Bcast	  
#define MPI_Barrier	   PMPI_Barrier	  
#define MPI_Comm_split     PMPI_Comm_split    
#define MPI_Send           PMPI_Send
#define MPI_Recv           PMPI_Recv
#endif

#include <sys/time.h>

#include <sys/types.h>
#include <fcntl.h>

#include <unistd.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"

#include "sion_mpi_cb_gen.h"

#ifdef SION_MPI

#define DFUNCTION "_mpi_gather_execute_cb"
int _sion_mpi_gather_process_cb(const void *indata, sion_int64 *spec, int spec_len, sion_int64 fsblksize,
				void *commdata,  int collector, int range_start, int range_end, int sid, 
				int process_cb(const void *,sion_int64 *, int ) ) {
  int       rc=SION_STD_SUCCESS;
  int       size, rank, t, startsignal=1;
  MPI_Status status;
  char      *p, *buffer; 
  sion_int64 bytestorecv, bytestosend, datasize;
  _mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;
  MPI_Comm   commp = sapi->comm;


  
  MPI_Comm_rank(commp, &rank);
  MPI_Comm_size(commp, &size);

  DPRINTFP((256, DFUNCTION, rank, " input collector=%d range_start=%d range_end=%d sid=%d\n", collector,range_start,range_end, sid));

  if(rank == collector) {
    /* its the collector */
 
    /* allocate buffer */
    buffer = (char *) malloc(fsblksize * sizeof(char));
    if (buffer == NULL) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_mpi_gather_process_cb: cannot allocate temporary memory of size %lu (buffer), aborting ...\n",
			      (unsigned long) fsblksize * sizeof(char)));
    }

    /* scan all other tasks */
    for(t=range_start;t<=range_end;t++) {

      /* receive spec */
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector wait for spec from %d\n", t));
      MPI_Recv(spec, spec_len, SION_MPI_INT64, t, 1534, commp, &status);
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector got spec from %d (%lld,%lld)\n", 
		t, (long long) spec[0], (long long) spec[1]));

      /* send signal to send data */
      if(spec[0]!=-1) { 	/* no error on sender */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector send signal to %d\n", t));
	MPI_Send(&startsignal, 1, MPI_INT, t, 1535, commp);
      }

      /* get and write data */
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector start to process data of size %lld at offset %lld\n", 
		(long long) spec[1], (long long) spec[0]));

      bytestorecv=spec[1];
      
      /* loop over data parts */
      while(bytestorecv>0) {
	if(bytestorecv>fsblksize) datasize=fsblksize;
	else                      datasize=bytestorecv;

	/* receive portion or all data */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector wait for data block from %d\n", t));
	MPI_Recv(buffer, datasize, MPI_CHAR, t, 1536, commp, &status);
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector got data block from %d datasize=%lld bytestorecv=%lld\n", 
		  t, (long long) datasize, (long long) bytestorecv));

	spec[1]=datasize; 	/* adjust size */

	/* process data with callback function */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector process data block of size %lld at pos %lld\n", 
		  (long long) spec[1], (long long) spec[0]));
	
	rc=process_cb(buffer,spec, sid);
	
	if(rc != SION_STD_SUCCESS) {
	  return(_sion_errorprint(SION_STD_NOT_SUCCESS,_SION_ERROR_RETURN,"_mpi_gather_process_cb: problems writing data ...\n"));
	}

	/* advance counter */
	bytestorecv-=datasize;spec[0]+=datasize;

      }
      
    }
    
    /* remove buffer */
    if (buffer) free(buffer);

  } else {
    if ( (rank>=range_start) && (rank<=range_end) ) {
      /* its a sender */
      
      /* send spec to collector */
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender send spec to %d (%lld,%lld)\n", 
		collector,(long long) spec[0], (long long) spec[1]));
      MPI_Send(spec, spec_len, SION_MPI_INT64, collector, 1534, commp);
      
      /* wait for start signal */
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender wait for signal from %d\n", collector));
      MPI_Recv(&startsignal, 1, MPI_INT, collector, 1535, commp, &status);
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender got signal from %d\n", collector));

      /* send data in chunks of fsblksize */
      bytestosend=spec[1];
      p=(char *) indata;
      while(bytestosend>0) {
	if(bytestosend>fsblksize) datasize=fsblksize;
	else                      datasize=bytestosend;
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender send data block to %d of size %lld\n", collector, (long long) datasize));
	MPI_Send(p, datasize, MPI_CHAR, collector, 1536, commp);
	bytestosend-=datasize;p+=datasize;
      }
    } else {
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "task take not part in collective operation\n"));
    } 
  }

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave collector=%d rc=%d\n", collector, rc ));

  return rc;
}
#undef DFUNCTION


#define DFUNCTION "_mpi_process_scatter_cb"
int _sion_mpi_process_scatter_cb(void *outdata, sion_int64 *spec, int spec_len, sion_int64 fsblksize,
				 void *commdata,  int collector, int range_start, int range_end, int sid, 
				 int process_cb(void *,sion_int64 *, int ) ) {
  int       rc=SION_STD_SUCCESS;
  int       size, rank, t, startsignal=1, count;
  MPI_Status status;
  char      *p, *buffer; 
  sion_int64 bytestorecv, bytestosend, datasize;
  _mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;
  MPI_Comm   commp = sapi->comm;

  
  MPI_Comm_rank(commp, &rank);
  MPI_Comm_size(commp, &size);

  DPRINTFP((256, DFUNCTION, rank, " input collector=%d range_start=%d range_end=%d sid=%d\n", collector,range_start,range_end, sid));

  if(rank == collector) {
    /* its the collector */
 
    /* allocate buffer */
    buffer = (char *) malloc(fsblksize * sizeof(char));
    if (buffer == NULL) {
      return(_sion_errorprint(SION_STD_NOT_SUCCESS,_SION_ERROR_RETURN,"_mpi_gather_process_cb: cannot allocate temporary memory of size %lu (buffer), aborting ...\n",
			      (unsigned long) fsblksize * sizeof(char)));
    }

    /* scan all other tasks */
    for(t=range_start;t<=range_end;t++) {

      /* receive spec */
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector wait for spec from %d\n", t));
      MPI_Recv(spec, spec_len, SION_MPI_INT64, t, 1534, commp, &status);
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector got spec from %d (%lld,%lld)\n", 
		t, (long long) spec[0], (long long) spec[1]));
      
      /* send signal to send data */
      if(spec[0]!=-1) { 	/*  sender waits for data */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector send signal to %d\n", t));
	MPI_Send(&startsignal, 1, MPI_INT, t, 1535, commp);
      }



      /* get and send data */
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector start to process data of size %lld at offset %lld\n", 
		(long long) spec[1], (long long) spec[0]));

      bytestosend=spec[1];
      
      /* loop over data parts */
      while(bytestosend>0) {

	if(bytestosend>fsblksize) datasize=fsblksize;
	else                      datasize=bytestosend;

	spec[1]=datasize; 	/* adjust size */

	/* process data with callback function */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector process data block of size %lld at pos %lld\n", 
		  (long long) spec[1], (long long) spec[0]));
	
	rc=process_cb(buffer,spec, sid);
	
	if(rc != SION_STD_SUCCESS) {
	  return(_sion_errorprint(SION_STD_NOT_SUCCESS,_SION_ERROR_RETURN,"_mpi_gather_process_cb: problems writing data ...\n"));
	}

	/* send portion or all data */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector send for data block to %d\n", t));
	MPI_Send(buffer, datasize, MPI_CHAR, t, 1536, commp);
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector sent data block to %d datasize=%lld bytestorecv=%lld\n", 
		  t, (long long) datasize, (long long) bytestosend));

	/* advance counter */
	bytestosend-=datasize;spec[0]+=datasize;

      }
      
    }
    
    /* remove buffer */
    if (buffer) free(buffer);

  } else {
    if ( (rank>=range_start) && (rank<=range_end) ) {
      /* its a sender */

      /* send spec to collector */
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender send spec to %d (%lld,%lld)\n", 
		collector,(long long) spec[0], (long long) spec[1]));
      MPI_Send(spec, spec_len, SION_MPI_INT64, collector, 1534, commp);

      if(spec[0]!=-1) { 		/* no error in sion_feof */

	/* wait for start signal */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender wait for signal from %d\n", collector));
	MPI_Recv(&startsignal, 1, MPI_INT, collector, 1535, commp, &status);
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender got signal from %d\n", collector));
      
	/* send data in chunks of fsblksize */
	bytestorecv=spec[1];
	p=(char *) outdata;
	while(bytestorecv>0) {
	  if(bytestorecv>fsblksize) datasize=fsblksize;
	  else                      datasize=bytestorecv;
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender recv data block from %d of size %lld\n", collector, (long long) datasize));
	  MPI_Recv(p, datasize, MPI_CHAR, collector, 1536, commp, &status);
	  MPI_Get_count(&status,MPI_CHAR,&count);
	
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender recv data block from %d of size %lld (%d)\n", collector, (long long) datasize, count));
	  bytestorecv-=datasize;p+=datasize;
	
	}
      }
    } else {
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "task take not part in collective operation\n"));
    }

  }

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave collector=%d rc=%d\n", collector, rc ));

  return rc;
}
#undef DFUNCTION


#define DFUNCTION "_sion_ompi_get_capability_cb"
int _sion_mpi_get_capability_cb(void *commdata )
{
  int       rc=SION_CAPABILITY_NONE;
  ONLY_DEBUG(_mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;)
  
  rc=SION_CAPABILITY_FULL;
  DPRINTFP((256, DFUNCTION, sapi->rank, "FULL capability\n"));
  return rc;
}
#undef DFUNCTION

/* end of ifdef MPI */
#endif
