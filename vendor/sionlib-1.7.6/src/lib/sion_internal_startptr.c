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

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <sys/time.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include <unistd.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_metadata.h"
#include "sion_filedesc.h"
#include "sion_tools.h"
#include "sion_fd.h"
#include "sion_file.h"
#include "sion_hints.h"
#include "sion_printts.h"
#include "sion_internal_startptr.h"
#include "sion_internal_collstat.h"

/* INTERNAL */

#define DFUNCTION "_sion_calculate_set_alignment"
sion_int64 _sion_calculate_set_alignment( _sion_filedesc *sion_filedesc, int t) {
  sion_int64 lsize=SION_SIZE_NOT_VALID;
  
  /* alignment: fsv 3, WRITE: -> align lsize  */
  /*          : fsv 3, READ:  -> align lsize  */
  /*          : fsv 4, WRITE: -> align lsize+chunksize  */
  /*          : fsv 4, READ:  -> lsize=chunksize  */
  
  if (sion_filedesc->fileversion<=3) {
    lsize = (sion_filedesc->all_chunksizes[t] % sion_filedesc->fsblksize == 0) ? 
             sion_filedesc->all_chunksizes[t] : 
            ((sion_filedesc->all_chunksizes[t] / sion_filedesc->fsblksize) + 1) * sion_filedesc->fsblksize;
    DPRINTFP((2048, DFUNCTION, _SION_DEFAULT_RANK, "old file version (<=3) task=%d lsize=%ld\n", t, (long) lsize));
  } else {
    if (sion_filedesc->mode==SION_FILEMODE_WRITE) {
      lsize = (sion_filedesc->all_chunksizes[t] % sion_filedesc->fsblksize == 0) ? 
	      sion_filedesc->all_chunksizes[t] : 
	     ((sion_filedesc->all_chunksizes[t] / sion_filedesc->fsblksize) + 1) * sion_filedesc->fsblksize;
      /* expand chunksize for alignment */
      sion_filedesc->all_chunksizes[t] = lsize; 
      DPRINTFP((2048, DFUNCTION, _SION_DEFAULT_RANK, "new file version (>3) task=%d lsize=%ld, WRITE chunksize set\n", t, (long) lsize));
    } else {
      lsize = sion_filedesc->all_chunksizes[t];
      DPRINTFP((2048, DFUNCTION, _SION_DEFAULT_RANK, "new file version (>3) task=%d lsize=%ld, READ no align\n", t, (long) lsize));
    }
  }

  return(lsize);
}
#undef DFUNCTION

/*!\brief Calculates the size of the first meta data block
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval size if ok, otherwise -1
 */
#define DFUNCTION "_sion_get_size_metadatablock1"
int _sion_get_size_metadatablock1( _sion_filedesc *sion_filedesc )
{
  int rc = 0;
  int firstsize;
  
  /* calculate size of first block for meta-information */
  firstsize = strlen(SION_ID) + 8 * sizeof(sion_int32)          /* length of SION_ID + endianness+fsblksize+ntasks        */
                                                                /*                   + nfiles+filenumber+3*versioninfo    */
    + SION_FILENAME_LENGTH                                      /* prefix                                                 */
    + sion_filedesc->ntasks * sizeof(sion_int64) + sion_filedesc->ntasks * sizeof(sion_int64) /* globalranks + chunksizes */
    + sizeof(sion_int32) + 3 * sizeof(sion_int64);              /* maxblocks + start_of_varheader + 2 x 64bit-flags       */
  rc=firstsize;

  return(rc);
}
#undef DFUNCTION

/*!\brief Calculate the start pointers
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval SION_SUCCESS if ok
 */
#define DFUNCTION "_sion_calculate_startpointers"
int _sion_calculate_startpointers( _sion_filedesc *sion_filedesc )
{

  int       rc = SION_SUCCESS;
  int       i, firstsize;
  sion_int64 lsize;

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "enter ntasks=%d fsblksize=%d chunksizes[0]=%lld\n", 
	       sion_filedesc->ntasks, sion_filedesc->fsblksize, sion_filedesc->all_chunksizes[0]));

  firstsize=_sion_get_size_metadatablock1(sion_filedesc);

#ifdef WFLARGEMETABLOCK
  if (firstsize<4*1024*1024) firstsize=4*1024*1024;
#endif

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "firstsize=%d\n", firstsize));

  sion_filedesc->all_startpointers[0] = (firstsize % sion_filedesc->fsblksize == 0) ? 
                                         firstsize : 
                                         ((firstsize / sion_filedesc->fsblksize) + 1) * sion_filedesc->fsblksize;

  i = 0;
  DPRINTFP((2048, DFUNCTION, _SION_DEFAULT_RANK, "  startpointers[%2d]=%10lld (%10.4fMB) chunksizes[%2d]=%8lld\n",
            i, sion_filedesc->all_startpointers[i], sion_filedesc->all_startpointers[i] / 1024.0 / 1024.0, i, sion_filedesc->all_chunksizes[i]));
  sion_filedesc->globalskip = 0;
  for (i = 1; i < sion_filedesc->ntasks; i++) {
    
    lsize=_sion_calculate_set_alignment(sion_filedesc,i-1);

    sion_filedesc->globalskip += lsize;
    sion_filedesc->all_startpointers[i] = sion_filedesc->all_startpointers[i - 1] + lsize;


    DPRINTFP((2048, DFUNCTION, _SION_DEFAULT_RANK, "  startpointers[%2d]=%10lld (%10.4fMB) chunksizes[%2d]=%8lld chunksizes[%2d]=%8lld\n",
              i, sion_filedesc->all_startpointers[i], sion_filedesc->all_startpointers[i] / 1024.0 / 1024.0, i, sion_filedesc->all_chunksizes[i], i-1, sion_filedesc->all_chunksizes[i-1]));
  }

  lsize=_sion_calculate_set_alignment(sion_filedesc,sion_filedesc->ntasks - 1);
  sion_filedesc->globalskip += lsize;

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "leave globalskip is %lld\n", sion_filedesc->globalskip));
  return (rc);
}
#undef DFUNCTION


/*!\brief Implements a heuiristic to determine the number of collectors per file
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 * @param  *num_collector                       
 *
 * @retval SION_SUCCESS if ok
 */
#define DFUNCTION "_sion_calculate_num_collector"
int _sion_calculate_num_collector( _sion_filedesc *sion_filedesc, _sion_collstat *collstat, int *num_collectors) {
  int        rc = SION_SUCCESS;
  int        max_num_collectors; 


  /* max. number: one fsblksize per collector */
  max_num_collectors  = (int) (collstat->gsize/sion_filedesc->fsblksize);
  if(collstat->gsize%sion_filedesc->fsblksize>0) max_num_collectors++;
  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "max_num_collectors=%d\n", max_num_collectors));
  
  if(sion_filedesc->collsize>0) {
    /* user defined collsize (number of tasks per collector) */
    *num_collectors = (int) (sion_filedesc->ntasks/sion_filedesc->collsize);
    if(sion_filedesc->ntasks%sion_filedesc->collsize>0) (*num_collectors)++;

    /* limit the user specification */
    if(*num_collectors > max_num_collectors) *num_collectors=max_num_collectors;                     

    DPRINTFP((32, DFUNCTION, _SION_DEFAULT_RANK, "user given collsize %d -> set num_collectors to %d\n",
	      sion_filedesc->collsize,*num_collectors));
  } else {

    /* determine number of collectors by heuristic */
    *num_collectors = max_num_collectors;

    /* limit the user specification */
    if(*num_collectors>sion_filedesc->ntasks) *num_collectors=sion_filedesc->ntasks;               
    
    /* some heuristics */
    if     ((sion_filedesc->ntasks>=512) && (*num_collectors>32)) *num_collectors=32;
    else if((sion_filedesc->ntasks>=256) && (*num_collectors>16)) *num_collectors=16;
    else if((sion_filedesc->ntasks>=128) && (*num_collectors>8))  *num_collectors=8;
    else if((sion_filedesc->ntasks>=64)  && (*num_collectors>8))  *num_collectors=8;
    else if((sion_filedesc->ntasks>=32)  && (*num_collectors>8))  *num_collectors=8;
    else if((sion_filedesc->ntasks>=16)  && (*num_collectors>4))  *num_collectors=4;

    /* TODO: check I/O infrastructure characteristics for better values */

    DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "SIONlib heuristic collsize=%d num_collectors=%d\n", 
	      sion_filedesc->collsize, *num_collectors));
  }

  /* data per collector */
  collstat->avg_data_per_collector = (
				      (sion_int64) (collstat->gsize / *num_collectors) 
				      / sion_filedesc->fsblksize
				      ) 
                                     * sion_filedesc->fsblksize; 

  if((collstat->gsize / *num_collectors)%sion_filedesc->fsblksize>0) collstat->avg_data_per_collector+=sion_filedesc->fsblksize;
  DPRINTFP((32, DFUNCTION, _SION_DEFAULT_RANK, "avg_data_per_collectors=%ld\n",(long) collstat->avg_data_per_collector));


  return(rc);
}
#undef DFUNCTION

/*!\brief Calculate the start pointers for collective operations
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval SION_SUCCESS if ok
 */
#define DFUNCTION "_sion_calculate_startpointers_collective"
int _sion_calculate_startpointers_collective( _sion_filedesc *sion_filedesc )
{

  int        rc = SION_SUCCESS;
  int        i, firstsize, num_collectors, numsender, lastcoll, s;

  sion_int64 currentsize, aligned_size, startpointer;
  _sion_collstat *collstat;

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "enter ntasks=%d fsblksize=%d chunksizes[0]=%lld\n", 
	       sion_filedesc->ntasks, sion_filedesc->fsblksize, sion_filedesc->all_chunksizes[0]));

  if (sion_filedesc->fileversion<=3) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
			    "_sion_calculate_startpointers_collective: files with old sionlib file format (<3) can not be read by collective calls, please use standard read calls, aborting ...\n"));
  }

  /* statistics */
  collstat=_sion_create_and_init_collstat(sion_filedesc);

  /* calculate size of first block for meta-information */
  collstat->firstsize=firstsize=_sion_get_size_metadatablock1(sion_filedesc);
  _sion_calculate_num_collector(sion_filedesc, collstat, &num_collectors);
  collstat->req_num_collectors=num_collectors; 

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "firstsize=%d collsize=%d num_collectors=%d\n", 
	    firstsize, sion_filedesc->collsize, num_collectors));

  numsender=1;lastcoll=0;
  currentsize=sion_filedesc->all_chunksizes[0];

  /* 1ST STEP: calculate mapping to collectors */
  for (i = 1; i < sion_filedesc->ntasks; i++) {
    if( (currentsize+sion_filedesc->all_chunksizes[i]<=collstat->avg_data_per_collector) || 
	(sion_filedesc->all_coll_capability[i]==SION_CAPABILITY_ONLY_SENDER ) ) {
      /* task will add to last collector */
      currentsize +=sion_filedesc->all_chunksizes[i];
      numsender++;
    } else {  /* new collector needed */

      /* adjust last senders */
      for(s=lastcoll;s<i;s++) {
	sion_filedesc->all_coll_collector[s]=lastcoll;
	sion_filedesc->all_coll_collsize[s] =numsender;
      }

      /* expand last one of group to align next one */
      if (sion_filedesc->mode==SION_FILEMODE_WRITE) {
	aligned_size = ((sion_int64) currentsize/sion_filedesc->fsblksize) * sion_filedesc->fsblksize; 
	if(currentsize%sion_filedesc->fsblksize>0) aligned_size+=sion_filedesc->fsblksize;

	DPRINTFP((128, DFUNCTION, _SION_DEFAULT_RANK, "  align chunksizes[%2d]=%8lld + %lld\n",
		  i-1, sion_filedesc->all_chunksizes[i-1], aligned_size-currentsize));

	sion_filedesc->all_chunksizes[i-1]+=aligned_size-currentsize;
      }

      /* init data for next collector */
      numsender    = 1;   lastcoll= i;  currentsize   = sion_filedesc->all_chunksizes[i];
    }
  }

  /* align last sender */
  if (sion_filedesc->mode==SION_FILEMODE_WRITE) {
    aligned_size = ((sion_int64) currentsize/sion_filedesc->fsblksize) * sion_filedesc->fsblksize; 
    if(currentsize%sion_filedesc->fsblksize>0) aligned_size+=sion_filedesc->fsblksize;
    DPRINTFP((128, DFUNCTION, _SION_DEFAULT_RANK, "  align chunksizes[%2d]=%8lld + %lld\n",
	      sion_filedesc->ntasks-1, sion_filedesc->all_chunksizes[sion_filedesc->ntasks-1], aligned_size-currentsize));
    sion_filedesc->all_chunksizes[sion_filedesc->ntasks-1]+=aligned_size-currentsize;
  }

  /* adjust sender of last collector */
  for(s=lastcoll;s<sion_filedesc->ntasks;s++) {
    sion_filedesc->all_coll_collector[s]=lastcoll;
    sion_filedesc->all_coll_collsize[s]=numsender;
  }

  /* 2ND STEP: calculate startpointers */

  /* align first, not necessary, only for debugging  */
  /* startpointer=firstsize; */
  startpointer = (firstsize % sion_filedesc->fsblksize == 0) ? firstsize : ((firstsize / sion_filedesc->fsblksize) + 1) * sion_filedesc->fsblksize;
  sion_filedesc->globalskip = 0;
  /* calculate mapping to collectors */
  for (i = 0; i < sion_filedesc->ntasks; i++) {
    sion_filedesc->all_startpointers[i] =  startpointer;
    startpointer                        += sion_filedesc->all_chunksizes[i];
    sion_filedesc->globalskip           += sion_filedesc->all_chunksizes[i];
  }

  /* statistics */
  if(sion_filedesc->colldebug>=1) {
    _sion_update_collstat(collstat,sion_filedesc);
    _sion_print_collstat(collstat, sion_filedesc);
  }
  
  _sion_debugprint_collstat(collstat, sion_filedesc);
  
  _sion_destroy_collstat(collstat);

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "leave globalskip is %lld\n", sion_filedesc->globalskip));
  return (rc);
}
#undef DFUNCTION

int _sion_calculate_startpointers_collective_msa(_sion_filedesc *fd)
{
  DPRINTFP((2, __func__, _SION_DEFAULT_RANK, "enter ntasks=%d fsblksize=%d chunksizes[0]=%lld\n", fd->ntasks, fd->fsblksize, fd->all_chunksizes[0]));

  if (fd->fileversion<=3) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
			    "_sion_calculate_startpointers_collective_msa: files with old sionlib file format (<3) can not be read by collective calls, please use standard read calls, aborting ...\n"));
  }

  sion_int64 firstsize = _sion_get_size_metadatablock1(fd);
  sion_int64 startpointer = (firstsize % fd->fsblksize == 0) ? firstsize : ((firstsize / fd->fsblksize) + 1) * fd->fsblksize;
  for (int i = 0; i < fd->ntasks; i++) {
    assert(i % fd->collsize != 0 || startpointer % fd->fsblksize == 0);
    fd->all_startpointers[i] = startpointer;
    fd->all_coll_collector[i] = (i / fd->collsize) * fd->collsize;
    if (fd->ntasks - i < fd->ntasks % fd->collsize) {
      fd->all_coll_collsize[i] = fd->ntasks % fd->collsize;
    } else {
      fd->all_coll_collsize[i] = fd->collsize;
    }
    if ((i % fd->collsize == fd->all_coll_collsize[i] - 1) && (fd->mode == SION_FILEMODE_WRITE)) {
      sion_int64 end_of_block = startpointer + fd->all_chunksizes[i];
      if (end_of_block % fd->fsblksize != 0) {
        fd->all_chunksizes[i] += fd->fsblksize - (end_of_block % fd->fsblksize);
      }
    }
    startpointer += fd->all_chunksizes[i];
    DPRINTFP((2, __func__, _SION_DEFAULT_RANK, "chunk: %d, collector: %d, collsize: %d, start: %lld, chunksize: %lld\n", i, fd->all_coll_collector[i], fd->all_coll_collsize[i], fd->all_startpointers[i], fd->all_chunksizes[i]));
  }

  fd->globalskip = startpointer - fd->all_startpointers[0];
  DPRINTFP((2, __func__, _SION_DEFAULT_RANK, "leave globalskip is %lld\n", fd->globalskip));
  assert(fd->globalskip % fd->fsblksize == 0);

  return SION_SUCCESS;
}

/*!\brief Calculate the start pointers for collective operations using special merge mode
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval SION_SUCCESS if ok
 */
#define DFUNCTION "_sion_calculate_startpointers_collective_merge"
int _sion_calculate_startpointers_collective_merge( _sion_filedesc *sion_filedesc )
{

  int        rc = SION_SUCCESS;
  int        i, firstsize, num_collectors, numsender, lastcoll, s;

  sion_int64 currentsize, newsize, aligned_size, startpointer;
  _sion_collstat *collstat;

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "enter ntasks=%d fsblksize=%d chunksizes[0]=%lld\n", 
	       sion_filedesc->ntasks, sion_filedesc->fsblksize, sion_filedesc->all_chunksizes[0]));

  if (sion_filedesc->fileversion<=3) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
			    "_sion_calculate_startpointers_collective: files with old sionlib file format (<3) can not be read by collective calls, please use standard read calls, aborting ...\n"));
  }

  /* statistics */
  collstat=_sion_create_and_init_collstat(sion_filedesc);

  /* calculate size of first block for meta-information */
  collstat->firstsize=firstsize=_sion_get_size_metadatablock1(sion_filedesc);
  _sion_calculate_num_collector(sion_filedesc, collstat, &num_collectors);
  collstat->req_num_collectors=num_collectors; 

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "firstsize=%d collsize=%d num_collectors=%d\n", 
	    firstsize, sion_filedesc->collsize, num_collectors));

  numsender=1;lastcoll=0;
  currentsize=sion_filedesc->all_chunksizes[0];

  DPRINTFP((128, DFUNCTION, _SION_DEFAULT_RANK, "  currentsize=%lld chunksizes[%2d]=%8lld\n",
	    currentsize, 0, sion_filedesc->all_chunksizes[0]));

  /* 1ST STEP: calculate mapping to collectors */
  for (i = 1; i < sion_filedesc->ntasks; i++) {
    if( (currentsize+sion_filedesc->all_chunksizes[i]<=collstat->avg_data_per_collector) || 
	(sion_filedesc->all_coll_capability[i]==SION_CAPABILITY_ONLY_SENDER ) ) {
      /* task will add to last collector */
      currentsize +=sion_filedesc->all_chunksizes[i];
      numsender++;
      DPRINTFP((128, DFUNCTION, _SION_DEFAULT_RANK, "  currentsize=%lld chunksizes[%2d]=%8lld\n",
		currentsize, i, sion_filedesc->all_chunksizes[i]));
    } else {  /* new collector needed */


      /* increase chunksize of last collector */
      if (sion_filedesc->mode==SION_FILEMODE_WRITE) {
	newsize=currentsize;

	/* expand chunksize to align next one */
	aligned_size = ((sion_int64) newsize/sion_filedesc->fsblksize) * sion_filedesc->fsblksize; 
	if(newsize%sion_filedesc->fsblksize>0) aligned_size+=sion_filedesc->fsblksize;
	
	DPRINTFP((128, DFUNCTION, _SION_DEFAULT_RANK, "  resize chunksizes[%2d]=%8lld + %lld\n",
		  lastcoll, sion_filedesc->all_chunksizes[lastcoll], aligned_size-sion_filedesc->all_chunksizes[lastcoll]));
	sion_filedesc->all_chunksizes[lastcoll]=aligned_size;

	for(s=lastcoll+1;s<i;s++) {
	  /* leave chunksize of sender on aligned original size to
	     allow storing data in intermeadiate non-collective
	     flushes */
	  _sion_calculate_set_alignment(sion_filedesc,s);

	  /* adjust chunksize of the sender to one file system block as spare space */
	  /* sion_filedesc->all_chunksizes[s]=sion_filedesc->fsblksize; */
	}

      }

      /* adjust last senders */
      for(s=lastcoll;s<i;s++) {
	sion_filedesc->all_coll_collector[s]=lastcoll;
	sion_filedesc->all_coll_collsize[s] =numsender;
      }
      
      /* init data for next collector */
      numsender    = 1;   lastcoll= i;  currentsize   = sion_filedesc->all_chunksizes[i];
    }
  }

  /* align last sender */
  if (sion_filedesc->mode==SION_FILEMODE_WRITE) {
	newsize=currentsize;
	
	/* expand chunksize to align next one */
	aligned_size = ((sion_int64) newsize/sion_filedesc->fsblksize) * sion_filedesc->fsblksize; 
	if(newsize%sion_filedesc->fsblksize>0) aligned_size+=sion_filedesc->fsblksize;
	
	DPRINTFP((128, DFUNCTION, _SION_DEFAULT_RANK, "  resize chunksizes[%2d]=%8lld + %lld\n",
		  lastcoll, sion_filedesc->all_chunksizes[lastcoll], aligned_size-sion_filedesc->all_chunksizes[lastcoll]));
	sion_filedesc->all_chunksizes[lastcoll]=aligned_size;

	/* adjust chunksize of the sender to one file system block as spare space */
	for(s=lastcoll+1;s<i;s++) {
	  /* leave chunksize of sender on aligned original size to
	     allow storing data in intermeadiate non-collective
	     flushes */
	  _sion_calculate_set_alignment(sion_filedesc,s);

	  /* adjust chunksize of the sender to one file system block as spare space */
	  /* sion_filedesc->all_chunksizes[s]=sion_filedesc->fsblksize; */
	}
  }

  /* adjust sender of last collector */
  for(s=lastcoll;s<sion_filedesc->ntasks;s++) {
    sion_filedesc->all_coll_collector[s]=lastcoll;
    sion_filedesc->all_coll_collsize[s]=numsender;
  }

  /* 2ND STEP: calculate startpointers */

  /* align first, not necessary, only for debugging  */
  /* startpointer=firstsize; */
  startpointer = (firstsize % sion_filedesc->fsblksize == 0) ? firstsize : ((firstsize / sion_filedesc->fsblksize) + 1) * sion_filedesc->fsblksize;
  sion_filedesc->globalskip = 0;
  /* calculate mapping to collectors */
  for (i = 0; i < sion_filedesc->ntasks; i++) {
    sion_filedesc->all_startpointers[i] =  startpointer;
    startpointer                        += sion_filedesc->all_chunksizes[i];
    sion_filedesc->globalskip           += sion_filedesc->all_chunksizes[i];
  }

  /* statistics */
  if(sion_filedesc->colldebug>=1) {
    _sion_update_collstat(collstat,sion_filedesc);
    _sion_print_collstat(collstat, sion_filedesc);
  }
  
  _sion_debugprint_collstat(collstat, sion_filedesc);
  
  _sion_destroy_collstat(collstat);

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "leave globalskip is %lld\n", sion_filedesc->globalskip));
  return (rc);
}
#undef DFUNCTION



