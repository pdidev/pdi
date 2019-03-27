/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*
 *
 * \brief Internal Functions(parallel)
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_file.h"
#include "sion_filedesc.h"
#include "sion_fd.h"
#include "sion_metadata.h"
#include "sion_internal.h"
#include "sion_printts.h"
#include "sion_flags.h"

#include "sion_buffer.h"

#include "sion_filedesc.h"
#include "sion_keyvalue.h"

#include "sion_generic_internal.h"
#include "sion_generic_collective.h"
#include "sion_generic_buddy.h"


/*!\brief Generic parallel open of a buddy checkpoint file
 *
 * @param  sid                   sion file handle
 * @param  fname                 filename to use
 * @param  file_mode_flags       flags from already parsed file_mode (see also sion_internal.h)
 * @param  prefix                prefix to be used in case of multiple files
 * @param  numFiles              Number of files to open
 * @param  filenumber            file number
 * @param  chunksize             chunksize on this task
 * @param  fsblksize             blocksize of filesystem (must be equal on all processors)
 * @param  rank                  rank of the current process
 * @param  ntasks                number of processes
 * @param  globalrank            any global unique id for this task, will be stored in sion file, usefull if comm is not MPI_COMM_WORLD
 *                               typical: globalrank= rank in MPI_COMM_WORLD
 * @param  flag                  sion options flag
 * @param  fileptr               filepointer for this task
 * @param  sion_gendata          structure, containing references to commgroups and callbacks
 *
 * @return  sion file handle
 *          -1 if error occured
 */
#define DFUNCTION "_sion_paropen_generic_buddy"
int _sion_paropen_generic_buddy(
				int    sid,
				const char  *fname,
				_sion_flags_store *flags_store,
				char  *prefix,
				int   *numFiles,
				int   *filenumber, 
				sion_int64  *chunksize,
				sion_int32  *fsblksize,
				int    rank,
				int    ntasks,
				int   *globalrank,
				FILE **fileptr,
				_sion_generic_gendata *sion_gendata )
{
  int rc = SION_SUCCESS;
  int b, capability, pass, buddylevel;
  sion_int32 file_globalrank;
  sion_int64  file_chunksize;
  char *buddy_fn, *nfname=NULL;
  _sion_filedesc        *sion_filedesc;
  _sion_generic_buddy   *buddies, *buddyptr;
  _sion_generic_apidesc *sion_apidesc;

  buddylevel = atoi(_sion_flags_get(flags_store,"buddy")->val);
  if (buddylevel==0) buddylevel=1; /* default */
  
  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "allocate memory for %d buddy levels\n", buddylevel));
  
  buddies = (_sion_generic_buddy *) malloc(buddylevel * sizeof(_sion_generic_buddy));
  if (buddies == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate buddies structure of size %lu (_sion_generic_buddy), aborting ...\n", 
			    (unsigned long) sizeof(_sion_generic_buddy)));
  }
  buddies->numgroups=0;

  buddy_fn = calloc(SION_FILENAME_LENGTH+10,1);
  if (buddy_fn == NULL) {
    free(buddies);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_buddy: cannot allocate temporary memory of size %lu (buddy_fn), aborting ...\n", 
			    (unsigned long) SION_FILENAME_LENGTH+10));
  }

  /* get pointer to internal datastructures */
  sion_apidesc=sion_gendata->apidesc;
  
  if (flags_store->mask&_SION_FMODE_WRITE) {
    
    /* STEP1: WRITE open normal file */
    nfname=(sion_apidesc->get_multi_filename_cb?sion_apidesc->get_multi_filename_cb:_sion_get_multi_filename)
      (fname,sion_gendata->filenumber);
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "call parallel open of %d files (current name %s)\n", 
	      sion_gendata->numfiles, nfname));
    rc=_sion_paropen_generic_one_file(sid, nfname, flags_store, prefix, 
				      &sion_gendata->numfiles, &sion_gendata->filenumber, 
				      chunksize, fsblksize, 
				      sion_gendata->lrank, sion_gendata->lsize, globalrank, 
				      _SION_INTERNAL_FLAG_BUDDY_NORMAL, fileptr, sion_gendata, NULL);
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "leave parallel open of %d files in #tasks=%d sid=%d globalrank=%d\n", sion_gendata->numfiles, 
	      sion_gendata->lsize, sid, sion_gendata->grank));
    free(nfname);
   
    /* test sid and get internal data structure */
    if ((rc<0) || (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
      free(buddies);
      free(buddy_fn);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_buddy: invalid sion_filedesc %d", sid));
    }
    
    /* store additional data */
    sion_filedesc->buddies=buddies;

    capability=sion_apidesc->get_capability_cb(sion_gendata->comm_data_global);

    /* open buddy files */
    for(b=0;b<sion_filedesc->buddylevel;b++)  {
      buddyptr=&buddies[b];

      /* create communicators */
      buddyptr->buddy_send.commgroup=NULL; buddyptr->buddy_coll.commgroup=NULL;
      rc=_sion_buddy_map(sion_gendata,capability,b+1,&buddyptr->buddy_send,&buddyptr->buddy_coll);
      
      /* file name */
      sprintf(buddy_fn,"%s_BUDDY_%02d", fname,b); 
           
      /* open files */
      for(pass=1;pass<=_SION_BW_SCHED_NUM_PASSES;pass++) {

	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO pass=%d [grank=%2d] op=%d\n",pass,*globalrank,
		  _sion_buddy_bwsched(buddyptr->buddy_send.groupnum, sion_gendata->numfiles, pass))); 


	switch (_sion_buddy_bwsched(buddyptr->buddy_send.groupnum, sion_gendata->numfiles, pass)) {
	case _SION_BW_SCHED_ACTIONA: 
	  /* parameter */
	  file_globalrank=-1*(*globalrank+1);
	  file_chunksize=0;
	  nfname=(sion_apidesc->get_multi_filename_cb?sion_apidesc->get_multi_filename_cb:_sion_get_multi_filename)
	    (buddy_fn,buddyptr->buddy_coll.filenum);
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO call open_one_file COLL [gendata: b=%d grank=%2d, group=%d of %d, lrank=%d of %d] fn=%s (%d)\n", 
		    b,*globalrank, buddyptr->buddy_coll.groupnum,sion_gendata->numfiles,
		    buddyptr->buddy_coll.rank, buddyptr->buddy_coll.size,nfname,buddyptr->buddy_coll.filenum));
	  buddyptr->buddy_coll.sid = _sion_newvcd(NULL,SION_FILEDESCRIPTOR);
	  rc=_sion_paropen_generic_one_file(buddyptr->buddy_coll.sid, nfname, flags_store, prefix, 
					    &sion_gendata->numfiles, &buddyptr->buddy_coll.filenum, 
					    &file_chunksize, fsblksize, 
					    buddyptr->buddy_coll.rank,buddyptr->buddy_coll.size, &file_globalrank, 
					    _SION_INTERNAL_FLAG_BUDDY_COLL, NULL, sion_gendata,
					    buddyptr);
	  free(nfname);
	  break;
	case _SION_BW_SCHED_ACTIONB: 
	  /* parameter */
	  file_globalrank=*globalrank;
	  file_chunksize=*chunksize;
	  nfname=(sion_apidesc->get_multi_filename_cb?sion_apidesc->get_multi_filename_cb:_sion_get_multi_filename)
	    (buddy_fn,buddyptr->buddy_send.filenum);
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO call open_one_file SEND [gendata: b=%d grank=%2d, group=%d of %d, lrank=%d of %d] fn=%s \n", 
		    b,*globalrank, buddyptr->buddy_send.groupnum,sion_gendata->numfiles,
		    buddyptr->buddy_send.rank, buddyptr->buddy_send.size,nfname));
	  buddyptr->buddy_send.sid = _sion_newvcd(NULL,SION_FILEDESCRIPTOR);
	  rc=_sion_paropen_generic_one_file(buddyptr->buddy_send.sid, nfname, flags_store, prefix, 
					    &sion_gendata->numfiles, &buddyptr->buddy_send.filenum, 
					    &file_chunksize, fsblksize, 
					    buddyptr->buddy_send.rank,buddyptr->buddy_send.size, &file_globalrank, 
					    _SION_INTERNAL_FLAG_BUDDY_SEND, NULL, sion_gendata,
					    buddyptr);
	  free(nfname);
	  break;
	default: 
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO no-op [grank=%2d]\n",*globalrank)); 
	  break;
	} /* switch */
      }	/* for pass */
    } /* for buddies */
 
  } else if (flags_store->mask&_SION_FMODE_READ) {
    int filefound, lgroup=*filenumber, root=0, task, tmpsize, group, masterfile_found; 
    sion_int32 filenumber, numfiles, lrank, lsize, file, file_lgroup, myrole, steprank;
    sion_int32 helpint32=0, numsteps=0, numgroups=0, numreaderinthisfile;
    sion_int32 *sion_tmpintfield1 = NULL; /* allocated on rank 0 */
    /* sion_int32 *sion_tmpintfield2 = NULL;  */  /* allocated on lranl-0 that has access to master file  */

    int *stepvectorlist[MAXREADSTEPS]; /* to be changed in an dynamic list */
    int *stepvector, readercount=0, readyflag=0, datafound=0; 
    int stepcount_coll,stepcount_collreader,stepcount_reader,stepcount_reader_rank,stepcount_noreader,stepcount_noreader_rank, groupcounter;
    int step,fit,newrank;
    _sion_generic_buddy_info *buddy_info;
    sion_int32 helpint7[7];

    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "enter buddy open in read mode\n"));

    if(sion_gendata->grank==0) {
      tmpsize=1*sion_gendata->gsize;
      sion_tmpintfield1 = (sion_int32 *) malloc(tmpsize * sizeof(sion_int32));
      if (sion_tmpintfield1 == NULL) {
        free(buddies);
        free(buddy_fn);
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_buddy: cannot allocate temporary memory of size %lu (sion_tmpintfield), aborting ...\n",
				(unsigned long) tmpsize * sizeof(sion_int32)));
      }
    }
 
    /* Loop1: over all file sets (normal, buddy1, buddy2) until all have found their data chunk */
    groupcounter=0;
    for(b=-1; ( (b<buddylevel) && (!readyflag));b++)  {

      /* determine filename and check for existence of master file on each lrank=0  */
      if(b==-1) sprintf(buddy_fn,"%s", fname); 
      else      sprintf(buddy_fn,"%s_BUDDY_%02d", fname,b); 
      nfname=(sion_apidesc->get_multi_filename_cb?sion_apidesc->get_multi_filename_cb:_sion_get_multi_filename)(buddy_fn,0);
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY[%2d] start checking buddy level (file=%s)\n",b,nfname));

      filefound=0;
      if(sion_gendata->lrank==0) {
	if(_sion_file_stat_file(nfname)) {
	  filefound=1;
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "check for existence of file %s --> file found\n",nfname));
	}
      }

      /* distribute info about master file to all others (root=<tasknum> or -1 if file not found) */
      helpint32=filefound;
      sion_gendata->apidesc->gatherr_cb(&helpint32, sion_tmpintfield1, sion_gendata->comm_data_global, _SION_INT32, 1, 0);
      if (sion_gendata->grank == 0) {
	root=-1;
        if (sion_tmpintfield1 == NULL) {
          free(buddies);
          free(buddy_fn);
          return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_ABORT, DFUNCTION ": gatherr_cb returned sion_tmpintfield1 == NULL"));
        }
        for(task=0;task<sion_gendata->gsize;task++) {
          if(sion_tmpintfield1[task]==1) root=task;
        }
        helpint32=root; 
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "after scan master: root for this file is %d #tasks=%d\n",root,sion_gendata->gsize));
      }
      sion_gendata->apidesc->bcastr_cb(&helpint32, sion_gendata->comm_data_global, _SION_INT32, 1, 0);  root=helpint32;
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "root for this file is %d\n",root));

      /* get mapping from file and distribute it */
      if(root!=-1) {
	_sion_generic_buddy_get_and_distribute_info_from_file(sion_gendata,nfname,root, &filenumber, &numfiles, &lrank, &lsize);
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY[%d] after read mapping file=%d of %d, lrank=%d of %d\n",b,filenumber, numfiles, lrank, lsize));
	masterfile_found=1;
      } else {
	filenumber=lrank=lsize=-1;
	masterfile_found=0;
      }
      free(nfname);

      /* numfiles will be defined from from input parameter, other info will be defined on-the-fly */
      numfiles=*numFiles;
      DPRINTFP((32, DFUNCTION, sion_gendata->grank, "Info avail: b=%d numfiles=%d root=%d lgroup=%d\n",b,numfiles,root,lgroup));

      /* Loop2: for each physical file of this file set */
      for(file=0;( (file<numfiles) && (!readyflag) );file++) {
	
	/* check physical file */
	nfname=(sion_apidesc->get_multi_filename_cb?sion_apidesc->get_multi_filename_cb:_sion_get_multi_filename)(buddy_fn,file);
	filefound=-1;
	if(sion_gendata->lrank==0) {
	  if(_sion_file_stat_file(nfname)) {
	    filefound=lgroup; 	
	    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "check for existence of file %s --> file found\n",nfname));
	  }
	}

	/* gather and distribute info (root and lgroup) about file to all others (root=<tasknum> or -1 if file not found) */
	helpint32=filefound;
	sion_gendata->apidesc->gatherr_cb(&helpint32, sion_tmpintfield1, sion_gendata->comm_data_global, _SION_INT32, 1, 0);
	if (sion_gendata->grank == 0) {
	  root=-1;
	  for(task=0;task<sion_gendata->gsize;task++) {
	    if(sion_tmpintfield1[task]>-1) {
	      root=task;
	      file_lgroup=sion_tmpintfield1[task];
	    }
	  }
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "after scan one file: root for this file is %d file_lgroup=%d\n",root,file_lgroup));
	}
	if (sion_gendata->grank == 0) helpint32=root; 
	sion_gendata->apidesc->bcastr_cb(&helpint32, sion_gendata->comm_data_global, _SION_INT32, 1, 0);  root=helpint32;
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "root for this file is %d\n",root));

	if(root>=0) {
	  /* file found */
	
	  if(!masterfile_found) {
	    /* get avail info from file: lrank/lsize for local tasks */
	    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY[%d] befre read globalranks file=%d of %d, lrank=%d of %d\n",
		      b, filenumber, numfiles, lrank, lsize));
	    _sion_generic_buddy_get_and_distribute_info_from_one_file(sion_gendata, nfname, root, &filenumber, &numfiles, &lrank, &lsize);
	    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY[%d] after read globalranks file=%d of %d, lrank=%d of %d\n",
		      b, filenumber, numfiles, lrank, lsize));
	  }

	  /* bcast lgroup */
	  if (sion_gendata->grank == 0) helpint32=file_lgroup; 
	  sion_gendata->apidesc->bcastr_cb(&helpint32, sion_gendata->comm_data_global, _SION_INT32, 1, 0);  file_lgroup=helpint32;
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "file_lgroup for this file is %d\n",file_lgroup));

	  /* determine my role for this file */
	  myrole=SION_ROLE_NONE; /* not acting on that file */
	  if((filenumber==file) &&  (!datafound)) {
	    datafound=1; 	/* fantastic ... */
	    if(sion_gendata->grank==root)                myrole=SION_ROLE_COLLECTOR_READER; /* collector and reader */
	    else                                         myrole=SION_ROLE_READER; /* reader */
	  } else {
	    if(sion_gendata->grank==root)                myrole=SION_ROLE_COLLECTOR; /* collector */
	    else if(file_lgroup==lgroup)                 myrole=SION_ROLE_NOREADER; /* task on the same lgroup but not collector */
	  }

	  /* collect on rank 0 info about each task acting on this file  */
	  helpint32=myrole;
	  sion_gendata->apidesc->gatherr_cb(&helpint32, sion_tmpintfield1, sion_gendata->comm_data_global, _SION_INT32, 1, 0);

	  /* check if at least one tasks needs data  */
	  if(sion_gendata->grank == 0) {
	    numreaderinthisfile=0;
            if (sion_tmpintfield1 == NULL) {
              return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_ABORT, DFUNCTION ": gatherr_cb returned sion_tmpintfield1 == NULL"));
            }
	    for(task=0; ( (task<sion_gendata->gsize) ) ;task++) 
	      if ( (sion_tmpintfield1[task]==SION_ROLE_COLLECTOR_READER) || (sion_tmpintfield1[task]==SION_ROLE_READER) ) numreaderinthisfile++;
	  }
	  sion_gendata->apidesc->bcastr_cb(&numreaderinthisfile, sion_gendata->comm_data_global, _SION_INT32, 1, 0); 

	  if (numreaderinthisfile>0) {

	    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "myrole for file %s is %s\n",nfname,_sion_buddy_role_to_str(myrole)));

	    /* check for step in which the group can be created */
	    if (sion_gendata->grank == 0) {
	      /* check in existing steps if group can created there */
	      fit=0;
	      for(step=0; ( (step<numsteps) && (fit==0) ) ;step++) {
		stepvector=stepvectorlist[step];
		fit=1;
		for(task=0; ( (task<sion_gendata->gsize) && (fit==1) ) ;task++) {
		  if(sion_tmpintfield1[task]!=SION_ROLE_NONE) {
		    if(stepvector[task]!=SION_ROLE_NONE) fit=0;
		  }
		}
	      }
	      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY STEP assign: after scanning %d steps fit=%d\n",numsteps,fit));
	      if(fit==0) {
		/* no space in available steps, create a new one */
		stepvector = (int *) malloc(sion_gendata->gsize * sizeof(int));
		if (stepvector == NULL) {
		  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_buddy: cannot allocate temporary memory of size %lu (stepvector), aborting ...\n",
					  (unsigned long) sion_gendata->gsize * sizeof(int)));
		}
		for(task=0; (task<sion_gendata->gsize);task++) stepvector[task]=SION_ROLE_NONE;
		numsteps++;step++;
		stepvectorlist[numsteps-1]=stepvector;
		DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY STEP assign: allocating new step %d\n",numsteps-1));
	      }
	      /* store roles in stepvector and analyse group */
	      stepcount_coll=stepcount_collreader=stepcount_reader=stepcount_noreader=0;
	      for(task=0; (task<sion_gendata->gsize);task++) {
		if(sion_tmpintfield1[task]!=SION_ROLE_NONE) {
		  stepvector[task]=sion_tmpintfield1[task];	
		  if (sion_tmpintfield1[task]==SION_ROLE_COLLECTOR)         stepcount_coll++;
		  if (sion_tmpintfield1[task]==SION_ROLE_READER)            {readercount++;stepcount_reader++;}
		  if (sion_tmpintfield1[task]==SION_ROLE_COLLECTOR_READER)  {readercount++;stepcount_collreader++;}
		  if (sion_tmpintfield1[task]==SION_ROLE_NOREADER)          stepcount_noreader++;
		}
	      }
	      /* DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY STEP assign: stepcount_coll=%d stepcount_collreader=%d stepcount_reader=%d stepcount_noreader=%d\n", */
	      /* 	  stepcount_coll,stepcount_collreader, stepcount_reader, stepcount_noreader)); */
	      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY STEP assign: group assigned to step %d (tasks with no data: %d)\n",numsteps-1,sion_gendata->gsize-readercount));
	      if(sion_gendata->gsize-readercount==0) readyflag=1;

	      /* calculate ranks */
	      stepcount_noreader_rank = stepcount_coll;
	      stepcount_reader_rank   = stepcount_noreader_rank+stepcount_noreader;
	      for(task=0; (task<sion_gendata->gsize);task++) {
		/*DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY STEP assign1: task %d stepcount_reader_rank=%d stepcount_noreader_rank=%d %s\n",
		  task, stepcount_reader_rank, stepcount_noreader_rank,_sion_buddy_role_to_str(sion_tmpintfield1[task])));*/
		if(sion_tmpintfield1[task]==SION_ROLE_NONE)             newrank=-1;
		if(sion_tmpintfield1[task]==SION_ROLE_COLLECTOR)        newrank=0;
		if(sion_tmpintfield1[task]==SION_ROLE_COLLECTOR_READER) newrank=stepcount_reader_rank++;
		if(sion_tmpintfield1[task]==SION_ROLE_NOREADER)         newrank=stepcount_noreader_rank++;
		if(sion_tmpintfield1[task]==SION_ROLE_READER)           newrank=stepcount_reader_rank++;
		sion_tmpintfield1[task]=newrank;
		/* DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDY STEP assign2: rank %d to task %d stepcount_reader_rank=%d stepcount_noreader_rank=%d\n",
		   sion_tmpintfield1[task],task, stepcount_reader_rank, stepcount_noreader_rank));*/
	      }
	    } /* if(rank==0) --> stepvector handling */

	      /* scatter rank info to tasks */
	    sion_gendata->apidesc->scatterr_cb(sion_tmpintfield1, &steprank, sion_gendata->comm_data_global, _SION_INT32, 1, 0);

	
	    /* bcast info ), ready flag, size, collsize, to_index  */
	    if (sion_gendata->grank == 0) {

	      helpint7[0]=step-1; /* step number */
	      helpint7[1]=readyflag; /* readyflag */
	      helpint7[2]=stepcount_coll+stepcount_collreader+stepcount_reader+stepcount_noreader; /* size */
	      helpint7[3]=stepcount_collreader+stepcount_reader; /* collsize */
	      helpint7[4]=stepcount_coll+stepcount_noreader; /* from_index */
	      helpint7[5]=stepcount_coll+stepcount_noreader+stepcount_collreader+stepcount_reader-1; /* to_index */
	      helpint7[6]=groupcounter++;	   /* unique Id of group */
	      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "define group #%d (step=%d, readyflag=%d, size=%d, collsize=%d, from=%d, to=%d) \n",
			helpint7[6], helpint7[0], helpint7[1], helpint7[2], helpint7[3], helpint7[4], helpint7[5]));
	    }
	    sion_gendata->apidesc->bcastr_cb(helpint7, sion_gendata->comm_data_global, _SION_INT32, 7, 0);  
	    readyflag=helpint7[1];

	    /* create new data structure for storing info about group if task is acting in this group */
	    if(myrole!=SION_ROLE_NONE) {
	      buddy_info = (_sion_generic_buddy_info *) malloc(sizeof(_sion_generic_buddy_info));
	      if (buddy_info == NULL) {
                free(buddies);
		return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_buddy: cannot allocate temporary memory of size %lu (buddy_info), aborting ...\n",
					(unsigned long) sizeof(_sion_generic_buddy_info)));
	      }
	      buddies->groups[buddies->numgroups]=buddy_info;  buddies->numgroups++;
	      buddy_info->groupid=helpint7[6];
	      buddy_info->stepnum=helpint7[0];
	      buddy_info->rank=steprank;
	      buddy_info->size=helpint7[2];
	      buddy_info->commgroup=NULL; /* will be created later */
	      buddy_info->collsize=helpint7[3]; 
	      buddy_info->from_index=helpint7[4]; 
	      buddy_info->to_index=helpint7[5]; 
	      buddy_info->myrole=myrole; 
	      buddy_info->bnum=b; 
	      if( (myrole==SION_ROLE_READER) || (myrole==SION_ROLE_COLLECTOR_READER)) {
		buddy_info->filelrank=lrank;
	      } else {
		buddy_info->filelrank=-1;
	      }
	      buddy_info->filenum=file;
	      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "create group #%d (step=%d, rank=%d, size=%d, collsize=%d, from=%2d, to=%2d) (bnum=%2d file# %2d) with role %s \n",
			buddy_info->groupid, buddy_info->stepnum, buddy_info->rank, buddy_info->size, buddy_info->collsize, 
			buddy_info->from_index, buddy_info->to_index, buddy_info->bnum, buddy_info->filenum, _sion_buddy_role_to_str(buddy_info->myrole)));

	    } /* create group */
	  } /* (numreaderinthisfile>0) */
	} /* if (file found on one of the lgroups) */
      } /* end of loop over physical files of file set */
    } /* end of loop over file sets (buddies) */
  
    if(!readyflag) {
      /* some tasks could not find their data */
      free(buddies);
      free(buddy_fn);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_buddy: cannot open file, not all files available"));
    }

    /* bcast max. number of steps */
    sion_gendata->apidesc->bcastr_cb(&numsteps, sion_gendata->comm_data_global, _SION_INT32, 1, 0);  
    buddies->numsteps=numsteps;

    /* bcast max. number of groups */
    if (sion_gendata->grank == 0) numgroups=groupcounter;
    sion_gendata->apidesc->bcastr_cb(&numgroups, sion_gendata->comm_data_global, _SION_INT32, 1, 0);  
    
    /* build commgroups */
    for(step=0;step<numsteps;step++) {
      /* search for group with this step */
      group=-1;
      for(b=0; ( (b<buddies->numgroups) && (group==-1) ); b++ ) {
	
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "check group #%d of %d in stepnum=%d  step=%d\n",b, buddies->numgroups, buddies->groups[b]->stepnum,step)); 
	if (buddies->groups[b]->stepnum==step) group=b;
      }

      if(group!=-1) {
	/* create group for step */
	buddy_info=buddies->groups[group];
	rc=sion_gendata->apidesc->create_lcg_cb(&buddy_info->commgroup,sion_gendata->comm_data_global,
						sion_gendata->grank,sion_gendata->gsize,
						buddy_info->rank,buddy_info->size,
						buddy_info->groupid,numgroups);
	DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "COMM_created active [grank=%2d of %2d, group=%2d of %2d, lrank=%2d of %2d]\n",
		  sion_gendata->grank,sion_gendata->gsize, buddy_info->groupid,numgroups, buddy_info->rank,buddy_info->size));
      } else {
	/* nothing to do in this step, create dummy comm */
	void *dummycommgroup=NULL;
	rc=sion_gendata->apidesc->create_lcg_cb(&dummycommgroup,sion_gendata->comm_data_global,
						sion_gendata->grank,sion_gendata->gsize,
						0,1,
						-1,numgroups);
	DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "COMM_created dummy  [grank=%2d of %2d, group=%2d of %2d, lrank=%2d of %2d]\n",
		  sion_gendata->grank,sion_gendata->gsize,
		  -1,numgroups,
		  0,1  ));
      }


      if(group!=-1) {
	/* open file */
	buddy_info=buddies->groups[group];
	buddies->currentgroup=group;

	if(buddy_info->bnum==-1) sprintf(buddy_fn,"%s", fname); 
	else                     sprintf(buddy_fn,"%s_BUDDY_%02d", fname,buddy_info->bnum); 
	nfname=(sion_apidesc->get_multi_filename_cb?sion_apidesc->get_multi_filename_cb:_sion_get_multi_filename)(buddy_fn,buddy_info->filenum);
	
	DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "open file for group #%d %s [step=%d grank=%2d of %2d, lrank=%2d of %2d filelrank=%2d]\n",
		  buddy_info->groupid,nfname,buddy_info->stepnum,
		  sion_gendata->grank,sion_gendata->gsize, buddy_info->rank,buddy_info->size,buddy_info->filelrank));

	/* test if tsk is a reader, a task is only in one step a reader */
	if( (buddy_info->myrole == SION_ROLE_READER) 
	    || (buddy_info->myrole == SION_ROLE_COLLECTOR_READER)) {
	  buddy_info->sid=sid; 	/* use already defined sid, this will be the master data structure */
	  rc=_sion_paropen_generic_one_file(buddy_info->sid, nfname, flags_store, prefix, 
					    &sion_gendata->numfiles, &buddy_info->filenum, 
					    chunksize, fsblksize, 
					    buddy_info->rank, buddy_info->size, &sion_gendata->grank, 
					    _SION_INTERNAL_FLAG_BUDDY_READ, NULL, sion_gendata,
					    buddies);

	  /* store internal buddy data structure */
	  /* test sid and get internal data structure */
	  if ((rc<0) || (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
            free(buddy_fn);
	    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_buddy: invalid sion_filedesc %d", sid));
	  }
	  
	  /* store additional data */
	  sion_filedesc->buddies=buddies;

	} else {
	  sion_int64  dummy_chunksize;
	  sion_int32  dummy_fsblksize;
	  buddy_info->sid = _sion_newvcd(NULL,SION_FILEDESCRIPTOR); /* create a new sid */
	  rc=_sion_paropen_generic_one_file(buddy_info->sid, nfname, flags_store, prefix, 
					    &sion_gendata->numfiles, &buddy_info->filenum, 
					    &dummy_chunksize, &dummy_fsblksize, 
					    buddy_info->rank, buddy_info->size, &sion_gendata->grank, 
					    _SION_INTERNAL_FLAG_BUDDY_READ, NULL, sion_gendata,
					    buddies);

	}

	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO afteropen [grank=%2d step=%d] rc=%d\n",sion_gendata->grank,step,rc)); 
	free(nfname);

      } else {
	/* nothing to do in this step */
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO no-op [grank=%2d]\n",sion_gendata->grank)); 
      }

      /* barrier between steps */
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO beforegbarrier [grank=%2d]\n",sion_gendata->grank)); 
      sion_apidesc->barrier_cb(sion_gendata->comm_data_global);
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO aftergbarrier [grank=%2d]\n",sion_gendata->grank)); 

      	  
    } /* for steps */


  } else {
    free(buddies);
    free(buddy_fn);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_buddy: unknown file mode"));
  }
  /* 'buddies' is still used later and should not be freed here. */
  /* free(buddies); */
  free(buddy_fn);
  sion_apidesc->barrier_cb(sion_gendata->comm_data_global);


  return(rc);
}
#undef DFUNCTION


/*!\brief Internal function to close parallel opened SION file and buddy files
 *
 * @param  sid  reference to file description struct (_sion_filedesc)
 * @param  rank  rank
 * @param  ntasks  number of tasks
 * @param  mapping_size  number of global tasks
 * @param  mapping  mapping
 * @param  sion_gendata  generic data struct
 *
 * @return  SION_SUCCESS if successful
 */
#define DFUNCTION "_sion_parclose_generic_buddy"
int _sion_parclose_generic_buddy(int sid,
				 int rank,
				 int ntasks,
				 int mapping_size,
				 sion_int32 *mapping,
				 _sion_generic_gendata *sion_gendata) {
  int rc=SION_SUCCESS;
  _sion_filedesc *sion_filedesc;
  /* _sion_generic_apidesc *sion_apidesc; */
  _sion_generic_buddy *buddies, *buddyptr;
  int         b_mapping_size=0,group,step;
  sion_int32 *b_mapping=NULL;
  _sion_generic_buddy_info *buddy_info;

  int b, pass;

  /* obtain pointers to internal data structures */
  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_parclose_generic_buddy: invalid sion_filedesc %d", sid));
  }
  /* sion_apidesc=sion_gendata->apidesc; */
  buddies=sion_filedesc->buddies;


  if (sion_filedesc->mode == SION_FILEMODE_WRITE) {
    
    /* close buddy files */
    for(b=0;b<sion_filedesc->buddylevel;b++)  {
      buddyptr=&buddies[b];
    

      /* collect mapping */
      _sion_generic_collect_mapping_buddy(buddyptr, sion_gendata, &b_mapping_size, &b_mapping);

    
      /* close files */
      for(pass=1;pass<=_SION_BW_SCHED_NUM_PASSES;pass++) {
      
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYC pass=%d [grank=%2d] op=%d\n",pass,sion_gendata->grank,
		  _sion_buddy_bwsched(buddyptr->buddy_send.groupnum, sion_gendata->numfiles, pass))); 
      
	switch (_sion_buddy_bwsched(buddyptr->buddy_send.groupnum, sion_gendata->numfiles, pass)) {
	case _SION_BW_SCHED_ACTIONA: 
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYc call parallel close COLL [gendata: grank=%2d, group=%d of %d, lrank=%d of %d]\n", 
		    sion_gendata->grank, buddyptr->buddy_coll.groupnum,sion_gendata->numfiles,
		    buddyptr->buddy_coll.rank, buddyptr->buddy_coll.size));
	  _sion_parclose_generic(buddyptr->buddy_coll.sid, buddyptr->buddy_coll.rank, buddyptr->buddy_coll.size, 
                                 b_mapping_size, b_mapping, _SION_INTERNAL_FLAG_BUDDY_COLL, sion_gendata, buddyptr );
	  break;
	case _SION_BW_SCHED_ACTIONB: 
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYc call parallel close SEND [gendata: grank=%2d, group=%d of %d, lrank=%d of %d]\n", 
		    sion_gendata->grank, buddyptr->buddy_send.groupnum,sion_gendata->numfiles,
		    buddyptr->buddy_send.rank, buddyptr->buddy_send.size));
	  _sion_parclose_generic(buddyptr->buddy_send.sid, buddyptr->buddy_send.rank, buddyptr->buddy_send.size, 
                                 0, NULL, _SION_INTERNAL_FLAG_BUDDY_SEND, sion_gendata, buddyptr );
	  break;
	default: 
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYC no-op [grank=%2d]\n",sion_gendata->grank)); 
	  break;
	} /* switch */
      }	/* for pass */
    } /* for buddies */
 
  
    _SION_SAFE_FREE(b_mapping, NULL);
    _SION_SAFE_FREE(buddies, NULL);


    /* STEP: close normal files */
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "call parallel close of %d files, sid=%d\n", 
	      sion_gendata->numfiles, sid));
    rc = _sion_parclose_generic( sid, sion_filedesc->rank, sion_filedesc->ntasks, mapping_size, mapping, _SION_INTERNAL_FLAG_NORMAL,
				 sion_gendata, NULL );
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "leave parallel close of %d files in #tasks=%d globalrank=%d\n", sion_gendata->numfiles, 
	      sion_gendata->lsize, sion_gendata->grank));
  
  } else if (sion_filedesc->mode == SION_FILEMODE_READ) {

    /* loop over all groups */
    for(step=0;step<buddies->numsteps;step++) {
      /* search for group with this step */
      group=-1;
      for(b=0; ( (b<buddies->numgroups) && (group==-1) ); b++ ) {
	
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "check group #%d of %d %d==%d  %x\n",b, buddies->numgroups, buddies->groups[b]->stepnum,step, buddies->groups[b])); 
	if (buddies->groups[b]->stepnum==step) group=b;
      }
      buddy_info=buddies->groups[group];
      
      if(group!=-1) {
	/* call parclose for this group */
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "call parallel close of %d files, sid=%d\n", 
		  sion_gendata->numfiles, sid));
	buddies->currentgroup=group;
	rc = _sion_parclose_generic( buddy_info->sid, buddy_info->rank,buddy_info->size, -1, NULL, _SION_INTERNAL_FLAG_BUDDY_READ,
				     sion_gendata, buddies );
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "leave parallel close of %d files in #tasks=%d globalrank=%d\n", sion_gendata->numfiles, 
		  sion_gendata->lsize, sion_gendata->grank));
      }
    } /* step */

    
  } else {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_parclose_generic_buddy: unknown file mode"));
  }
  

  return(rc);
}
#undef DFUNCTION


/*!\brief Internal function to write data to buddy checkpoint file
 *
 * @param  ...
 * @param  sid  reference to file description struct (_sion_filedesc)
 * @param  sion_gendata  generic data struct
 *
 * @return  SION_SUCCESS if successful
 */
#define DFUNCTION "_sion_coll_fwrite_buddy"
int _sion_coll_fwrite_buddy(const void *data, 
			    size_t size, 
			    size_t nitems, 
			    int sid,
			    _sion_generic_gendata *sion_gendata ) {
  int rc=SION_SUCCESS;
  _sion_filedesc *sion_filedesc,*sion_filedesc_coll,*sion_filedesc_send;
  _sion_generic_apidesc *sion_apidesc;
  _sion_generic_buddy *buddies, *buddyptr;
  sion_int64  spec[2], ownnewposition;
  int b, pass, collector=0;

  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "enter parallel write buddy\n"));
  
  /* obtain pointers to internal data structures */
  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_coll_fwrite_buddy: invalid sion_filedesc %d", sid));
  }
  sion_apidesc=sion_gendata->apidesc;
  buddies=sion_filedesc->buddies;

  /* buddy files */
  for(b=0;b<sion_filedesc->buddylevel;b++)  {
    buddyptr=&buddies[b];
    
    /* get pointer to data structures */
    if ( (buddyptr->buddy_coll.sid<0) || (_sion_vcdtype(buddyptr->buddy_coll.sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc_coll = _sion_vcdtovcon(buddyptr->buddy_coll.sid))) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_coll_fwrite_buddy: invalid sion_filedesc %d", buddyptr->buddy_coll.sid));
    }
    if ( (buddyptr->buddy_send.sid<0) || (_sion_vcdtype(buddyptr->buddy_send.sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc_send = _sion_vcdtovcon(buddyptr->buddy_send.sid))) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_coll_fwrite_buddy: invalid sion_filedesc %d", buddyptr->buddy_send.sid));
    }


    /* open files */
    for(pass=1;pass<=_SION_BW_SCHED_NUM_PASSES;pass++) {
      
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYw pass=%d [grank=%2d] op=%d\n",pass,sion_gendata->grank,
		_sion_buddy_bwsched(buddyptr->buddy_send.groupnum, sion_gendata->numfiles, pass))); 
      
      switch (_sion_buddy_bwsched(buddyptr->buddy_send.groupnum, sion_gendata->numfiles, pass)) {
      case _SION_BW_SCHED_ACTIONA: 
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYw call parallel write COLL [gendata: grank=%2d, group=%d of %d, lrank=%d of %d] %d..%d\n", 
		  sion_gendata->grank, buddyptr->buddy_coll.groupnum,sion_gendata->numfiles,
		  buddyptr->buddy_coll.rank, buddyptr->buddy_coll.size,buddyptr->buddy_coll.from_index,buddyptr->buddy_coll.to_index));
	spec[0]=0;spec[1]=0;  
	if(sion_filedesc_coll->rank == collector) {
	  ownnewposition=sion_filedesc_coll->currentpos;
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYw call parallel write COLL currentpos=%d\n",(int) sion_filedesc_coll->currentpos)); 
	}
	rc = sion_apidesc->gather_execute_cb(data,spec,2, sion_filedesc_coll->fsblksize,
					     buddyptr->buddy_coll.commgroup,collector,buddyptr->buddy_coll.from_index,buddyptr->buddy_coll.to_index,
					     buddyptr->buddy_coll.sid, _sion_generic_collective_process_write);
	if(sion_filedesc_coll->rank == collector) {
	  _sion_file_flush(sion_filedesc_coll->fileptr);
	  _sion_file_set_position(sion_filedesc_coll->fileptr,ownnewposition);sion_filedesc_coll->currentpos=ownnewposition;
	  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYw call parallel write COLL currentpos=%d\n",(int) sion_filedesc_coll->currentpos)); 
	}
	break;
      case _SION_BW_SCHED_ACTIONB: 
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYw call parallel write SEND [gendata: grank=%2d, group=%d of %d, lrank=%d of %d] %d..%d\n", 
		  sion_gendata->grank, buddyptr->buddy_send.groupnum,sion_gendata->numfiles,
		  buddyptr->buddy_send.rank, buddyptr->buddy_send.size,buddyptr->buddy_send.from_index,buddyptr->buddy_send.to_index));

	/* ensure free space for this block */
	if(sion_ensure_free_space(buddyptr->buddy_send.sid,size*nitems) != SION_SUCCESS) {
	  _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"could not ensure free space for this block, returning %d ...\n", sid);
	  spec[0]=spec[1]=-1; 	/* signaling error */
	} else {
	  spec[0]=sion_filedesc_send->currentpos;
	  spec[1]=size*nitems;
	}
	rc = sion_apidesc->gather_execute_cb(data,spec,2, sion_filedesc_send->fsblksize,
					     buddyptr->buddy_send.commgroup,collector,buddyptr->buddy_send.from_index,buddyptr->buddy_send.to_index,
					     buddyptr->buddy_send.sid, _sion_generic_collective_process_write);
	sion_filedesc_send->currentpos+=size*nitems;
	break;
      default: 
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYw no-op [grank=%2d]\n",sion_gendata->grank)); 
	break;
      } /* switch */
    }	/* for pass */
  } /* for buddies */
  
  
  /* sion_apidesc->barrier_cb(sion_gendata->comm_data_global); */

  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "leave parallel write buddy\n"));
  
  

  return(rc);
}
#undef DFUNCTION


/*!\brief Internal function to read data from buddy checkpoint file
 *
 * @param  ...
 * @param  sid  reference to file description struct (_sion_filedesc)
 * @param  sion_gendata  generic data struct
 *
 * @return  SION_SUCCESS if successful
 */
#define DFUNCTION "_sion_coll_fread_buddy"
int _sion_coll_fread_buddy( void *data, size_t size, size_t nitems, int sid) {
  _sion_filedesc        *sion_filedesc,*b_sion_filedesc;
  _sion_generic_gendata *sion_gendata;
  _sion_generic_apidesc *sion_apidesc;
  sion_int64             bread=-1, spec[2], ownnewposition, items_read = 0;
  int                    rc_own=SION_STD_SUCCESS,rc_cb=SION_STD_SUCCESS;
  int                    collector, firstsender, lastsender, step, b, group;
  _sion_generic_buddy    *buddies;
  _sion_generic_buddy_info *buddy_info;

  /* obtain pointers to internal data structures */
  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_coll_fread_buddy: invalid sion_filedesc %d", sid));
  }
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "enter usecoll=%d collector=%d collsize=%d (%d tasks, %d files)\n", 
	    sion_filedesc->usecoll, sion_filedesc->collector, sion_filedesc->collsize, sion_filedesc->ntasks,sion_filedesc->nfiles));

  sion_gendata=sion_filedesc->dataptr;
  sion_apidesc=sion_gendata->apidesc;
  buddies=sion_filedesc->buddies;
  
  /* needed for avoiding subsequent non-collective calls */
  sion_filedesc->collcmdused=1;

    /* check collsize */
  if (sion_filedesc->collsize<=0) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_coll_fread_buddy: collsize=%d <= 0, returning ...\n", 
			    (int) sion_filedesc->collsize));
  }


  /* loop over all groups */
  for(step=0;step<buddies->numsteps;step++) {
    /* search for group with this step */
    group=-1;
    for(b=0; ( (b<buddies->numgroups) && (group==-1) ); b++ ) {
      
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "check group #%d of %d %d==%d  %x\n",b, buddies->numgroups, buddies->groups[b]->stepnum,step, buddies->groups[b])); 
      if (buddies->groups[b]->stepnum==step) group=b;
    }
    
    if(group!=-1) {
      /* activity needed for this group */

      buddy_info=buddies->groups[group];
      
      if ( (buddy_info->sid<0) || (_sion_vcdtype(buddy_info->sid) != SION_FILEDESCRIPTOR) || !(b_sion_filedesc = _sion_vcdtovcon(buddy_info->sid))) {
	return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_coll_fread_buddy: invalid sion_filedesc %d", buddy_info->sid));
      }
      
      /* parameter of callback function */
      collector=0;
      firstsender=buddy_info->from_index;
      lastsender=buddy_info->to_index;
      

      /* check input parameter and position in file  */
      if( (buddy_info->myrole == SION_ROLE_READER) 
	  || (buddy_info->myrole == SION_ROLE_COLLECTOR_READER)) {
	
	/* ensure to be at the beginning of the right block */
	if(size*nitems>0) {
	  if(sion_feof(buddy_info->sid)) {
	    _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"early eof found for this block, returning %d ...\n", buddy_info->sid);
	    spec[0]=spec[1]=-1; 	/* signaling that no data is requested */
	  } else {
	    /* specification of location in file  */
	    spec[0]=sion_filedesc->currentpos;
	    spec[1]=size*nitems;
	  }
	} else {
	  /* signaling that no data is requested */
	  spec[0]=spec[1]=-1; 	
	}


      }
      
      
      /* read own part */
      if( (buddy_info->myrole == SION_ROLE_COLLECTOR_READER) ) {
	rc_own=_sion_generic_collective_process_read(data,spec,buddy_info->sid);
	firstsender++; 		/* must not read again */
      }
      /* save own position before reading at other positions */
      if( (buddy_info->myrole == SION_ROLE_COLLECTOR) 
	  || (buddy_info->myrole == SION_ROLE_COLLECTOR_READER)) {
	ownnewposition=sion_filedesc->currentpos;
      }
      
      
      /* read parts and scatter these to sender tasks via callback function */
      if(!sion_apidesc->execute_scatter_cb ) {
	return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,
				"_sion_coll_fread_buddy: API %s not correctly initalized, collective I/O calls missing, aborting",sion_apidesc->name));
      }
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "call execute_gather #%d (step=%d, rank=%d, size=%d, collsize=%d, from=%2d, to=%2d) (bnum=%2d file# %2d) with role %s \n",
		buddy_info->groupid, buddy_info->stepnum, buddy_info->rank, buddy_info->size, buddy_info->collsize, 
		buddy_info->from_index, buddy_info->to_index, buddy_info->bnum, buddy_info->filenum, _sion_buddy_role_to_str(buddy_info->myrole)));
      
      rc_cb=sion_apidesc->execute_scatter_cb(data,spec,2, b_sion_filedesc->fsblksize,
					     buddy_info->commgroup,collector,firstsender,lastsender,buddy_info->sid, 
					     _sion_generic_collective_process_read);
      
      
      
      /* set own position to end of own block read in this call */ 
      if( (buddy_info->myrole == SION_ROLE_COLLECTOR) 
	  || (buddy_info->myrole == SION_ROLE_COLLECTOR_READER)) {
	_sion_file_flush(b_sion_filedesc->fileptr);
	_sion_file_set_position(b_sion_filedesc->fileptr,ownnewposition);b_sion_filedesc->currentpos=ownnewposition;
      }
      
      /* set file pointer in data structure and in file if it is exported and can be used without control of SIONlib */
      if(buddy_info->myrole == SION_ROLE_READER) {
	b_sion_filedesc->currentpos+=size*nitems;
	if(b_sion_filedesc->fileptr_exported) {
	_sion_file_set_position(b_sion_filedesc->fileptr,b_sion_filedesc->currentpos);
	}
      }
    
      if( (buddy_info->myrole == SION_ROLE_READER) 
	  || (buddy_info->myrole == SION_ROLE_COLLECTOR_READER)) {
	if( (rc_own == SION_STD_SUCCESS) && (rc_cb == SION_STD_SUCCESS) ) {
	  bread=size*nitems;
	} else {
	  bread=0;
	}
	items_read = size ? bread / size : 0;
      }

    } /* froup != -1 */

    /* barrier between steps */
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO beforegbarrier [grank=%2d]\n",sion_gendata->grank)); 
    sion_apidesc->barrier_cb(sion_gendata->comm_data_global);
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "BUDDYO aftergbarrier [grank=%2d]\n",sion_gendata->grank)); 

  } /* for step */

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave usecoll=%d collector=%d collsize=%d (%d tasks, %d files) rc=%d\n", 
	    sion_filedesc->usecoll, sion_filedesc->collector, sion_filedesc->collsize, sion_filedesc->ntasks,sion_filedesc->nfiles,items_read));

  return items_read;
  
}
#undef DFUNCTION

#define DFUNCTION "_sion_buddy_map"
int _sion_buddy_map(
		    _sion_generic_gendata *sion_gendata,
		    int capability,
		    int buddy_idx,
		    _sion_generic_buddy_info *buddy_send,
		    _sion_generic_buddy_info *buddy_coll ) {
  int rc=SION_SUCCESS;
  int tmpsize,t,g, g_map, p, p_map, n, n_map, orig, distance,distance_1step,pass;
  int *size_per_group = NULL, *group_map_orig_to_new, *group_map_new_to_orig, mynewgroupnr;
  sion_int32 helpint2[2];
  sion_int32 *tasktogroup  = NULL;
  sion_int32 *tmpintfield1 = NULL;
  int comm_send_rank, comm_send_size, comm_send_grpnum; 
  int comm_coll_rank, comm_coll_size, comm_coll_grpnum; 
  void *dummycommgroup=NULL;

  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "buddy_idx=%d capability=%d [gendata: grank=%d of %d, group=%d of %d, lrank=%d of %d]\n", 
	    buddy_idx, capability,
	    sion_gendata->grank, sion_gendata->gsize, 
	    sion_gendata->filenumber, sion_gendata->numfiles, 
	    sion_gendata->lrank, sion_gendata->lsize));

  distance=buddy_idx; 		/* higher level buddy CP: groups will be remapped to d=1 buddy CP  */
  distance_1step=1; 

  /* allocate some fields */
  if(sion_gendata->grank==0) {

    tmpsize=1*sion_gendata->gsize;
    tasktogroup = (sion_int32 *) malloc(tmpsize * sizeof(sion_int32));
    if (tasktogroup == NULL) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_buddy_map: cannot allocate temporary memory of size %lu (tasktogroup), aborting ...\n",
			      (unsigned long) tmpsize * sizeof(sion_int32)));
    }

    tmpsize=2*sion_gendata->gsize;
    tmpintfield1 = (sion_int32 *) malloc(tmpsize * sizeof(sion_int32));
    if (tmpintfield1 == NULL) {
      free(tasktogroup);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_buddy_map: cannot allocate temporary memory of size %lu (tmpintfield1), aborting ...\n",
			      (unsigned long) tmpsize * sizeof(sion_int32)));
    }

    size_per_group = (int *) malloc(sion_gendata->numfiles * sizeof(int));
    if (size_per_group == NULL) {
      free(tasktogroup);
      free(tmpintfield1);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_buddy_map: cannot allocate temporary memory of size %lu (size_per_group), aborting ...\n",
			      (unsigned long) sion_gendata->numfiles * sizeof(int)));
    }

  }

  /* on all tasks */
  group_map_orig_to_new = (int *) malloc(sion_gendata->numfiles * sizeof(int));
  if (group_map_orig_to_new == NULL) {
    free(tasktogroup);
    _SION_SAFE_FREE(size_per_group, NULL);
    free(tmpintfield1);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_buddy_map: cannot allocate temporary memory of size %lu (group_map_orig_to_new), aborting ...\n",
			    (unsigned long) sion_gendata->numfiles * sizeof(int)));
  }
  
  group_map_new_to_orig = (int *) malloc(sion_gendata->numfiles * sizeof(int));
  if (group_map_new_to_orig == NULL) {
    _SION_SAFE_FREE(size_per_group, NULL);
    free(group_map_orig_to_new);
    free(tasktogroup);
    free(tmpintfield1);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_buddy_map: cannot allocate temporary memory of size %lu (group_map_new_to_orig), aborting ...\n",
			    (unsigned long) sion_gendata->numfiles * sizeof(int)));
  }

  /* Step1: gather group number */
  helpint2[0]=sion_gendata->filenumber;
  sion_gendata->apidesc->gatherr_cb(helpint2, tasktogroup, sion_gendata->comm_data_global, _SION_INT32,1,0);


  /* Step2: gather local size */
  helpint2[0]=sion_gendata->lsize;
  helpint2[1]=capability;
  sion_gendata->apidesc->gatherr_cb(helpint2, tmpintfield1, sion_gendata->comm_data_global, _SION_INT32,2,0);
  if(sion_gendata->grank==0) {
    for(t=0;t<sion_gendata->gsize;t++) {
      DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "task-map --> t=%2d grpnr=%2d lsize=%2d capability=%d\n",
		t,tasktogroup[t],tmpintfield1[2*t+0],tmpintfield1[2*t+1]));
    }
    /* --> tasktogroup[t] = groupnr,  tmpintfield1[t][0] = lsize tmpintfield1[t][1] = capability  */
  }

  /* Step2: build vector with size info per group and group map */
  if(sion_gendata->grank==0) {
    for(t=0;t<sion_gendata->gsize;t++) {
      size_per_group[tasktogroup[t]] = tmpintfield1[2*t+0];
    }
  }

  for(g=0;g<sion_gendata->numfiles;g++) {
    orig=(g*distance) % sion_gendata->numfiles;
    group_map_orig_to_new[orig]=g;
    group_map_new_to_orig[g]=orig;
  }
  
  for(g=0;g<sion_gendata->numfiles;g++) {
    DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "grp-map --> g=%2d->%2d\n",g,group_map_orig_to_new[g]));
  }
  
  
  /* Step3: compute send group */
  if(sion_gendata->grank==0) {
    for(t=0;t<sion_gendata->gsize;t++) {
      g=tasktogroup[t];
      g_map=group_map_orig_to_new[g];
      p_map=(g_map-distance_1step + sion_gendata->numfiles) % sion_gendata->numfiles;
      p=group_map_new_to_orig[p_map];
      tmpintfield1[2*t+0] = g_map;
      tmpintfield1[2*t+1] = size_per_group[p];
      DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "comm-send --> t=%2d grpnr=%2d grpmap=%2d prevgr=%2d prevgrmap=%2d prev_lsize=%2d\n",
		t, g, tmpintfield1[2*t+0], p, p_map, tmpintfield1[2*t+1]));
    }
    /* --> tmpintfield1[t][0] = own_groupnr,  tmpintfield1[t][1] = lsize of prev_group  */
  }
  
  /* Step4: scatter info about send group */
  sion_gendata->apidesc->scatterr_cb(tmpintfield1, helpint2, sion_gendata->comm_data_global, _SION_INT32,2,0);

  /* Step5: compute ranks for send group */
  mynewgroupnr=comm_send_grpnum=helpint2[0];
  buddy_send->rank=comm_send_rank=helpint2[1]+sion_gendata->lrank;
  buddy_send->size=comm_send_size=helpint2[1]+sion_gendata->lsize;
  buddy_send->collsize=sion_gendata->lsize;
  buddy_send->groupnum=mynewgroupnr;
  buddy_send->filenum=group_map_new_to_orig[mynewgroupnr];
  buddy_send->from_index=helpint2[1];
  buddy_send->to_index=helpint2[1]+sion_gendata->lsize-1;
  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "COMM_SEND d=%d grpnum=%2d lrank=%2d lsize=%2d mynewgroupnr=%2d collsize=%2d %d..%d fnum=%d\n",
	    distance,comm_send_grpnum,comm_send_rank,comm_send_size,mynewgroupnr,
	    buddy_send->collsize,buddy_send->from_index,buddy_send->to_index,
	    buddy_send->filenum));

  /* Step6: compute coll group */
  if(sion_gendata->grank==0) {
    for(t=0;t<sion_gendata->gsize;t++) {
      g=tasktogroup[t];
      g_map=group_map_orig_to_new[g];
      n_map=(g_map+distance_1step) % sion_gendata->numfiles;
      n=group_map_new_to_orig[n_map];
      tmpintfield1[2*t+0] = n_map;
      tmpintfield1[2*t+1] = size_per_group[n];
      DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "comm-coll --> t=%2d grpnr=%2d grpmap=%2d nextgr=%2d nextgrmap=%2d next_lsize=%2d\n",
		t, g, g_map, n, tmpintfield1[2*t+0], tmpintfield1[2*t+1]));
    }
    /* --> tmpintfield1[t][0] = next_groupnr_map, tmpintfield1[t][0] = lsize of next_group  */
  }

  /* Step7: scatter info about coll group */
  sion_gendata->apidesc->scatterr_cb(tmpintfield1, helpint2, sion_gendata->comm_data_global, _SION_INT32,2,0);

  /* Step8: compute ranks for coll group */
  comm_coll_grpnum=helpint2[0];
  buddy_coll->rank=comm_coll_rank=sion_gendata->lrank;
  buddy_coll->size=comm_coll_size=sion_gendata->lsize+helpint2[1];
  buddy_coll->collsize=helpint2[1];
  buddy_coll->groupnum=comm_coll_grpnum;
  buddy_coll->filenum=group_map_new_to_orig[comm_coll_grpnum];
  buddy_coll->from_index=sion_gendata->lsize;
  buddy_coll->to_index=sion_gendata->lsize+helpint2[1]-1;
  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "COMM_COLL d=%d grpnum=%2d lrank=%2d lsize=%2d collsize=%2d %d..%d fnum=%d\n",
	    distance,comm_coll_grpnum,comm_coll_rank,comm_coll_size,
	    buddy_coll->collsize,buddy_coll->from_index,buddy_coll->to_index,
	    buddy_coll->filenum));

  /* Step9: create local communicators for send/coll */
  
  for(pass=1;pass<=_SION_BW_SCHED_NUM_PASSES;pass++) {
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "pass=%d [grank=%2d] op=%d\n",pass,sion_gendata->grank,
	      _sion_buddy_bwsched(mynewgroupnr, sion_gendata->numfiles, pass))); 
    
    switch (_sion_buddy_bwsched(mynewgroupnr, sion_gendata->numfiles, pass)) {
    case _SION_BW_SCHED_ACTIONA: 
      rc=sion_gendata->apidesc->create_lcg_cb(&buddy_coll->commgroup,sion_gendata->comm_data_global,
					      sion_gendata->grank,sion_gendata->gsize,
					      comm_coll_rank,comm_coll_size,
					      comm_coll_grpnum,sion_gendata->numfiles);
      DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "COMM_created comm_coll (d=%1d, p=%1d) [gendata: mygrp=%2d grank=%2d of %2d, group=%2d of %2d, lrank=%2d of %2d]\n",
		distance,pass,mynewgroupnr,sion_gendata->grank,sion_gendata->gsize,
		comm_coll_grpnum,sion_gendata->numfiles,
		comm_coll_rank,comm_coll_size  ));
      break;
    case _SION_BW_SCHED_ACTIONB: 
      rc=sion_gendata->apidesc->create_lcg_cb(&buddy_send->commgroup,sion_gendata->comm_data_global,
					      sion_gendata->grank,sion_gendata->gsize,
					      comm_send_rank,comm_send_size,
					      comm_send_grpnum,sion_gendata->numfiles);
      DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "COMM_created comm_send (d=%1d, p=%1d) [gendata: mygrp=%2d grank=%2d of %2d, group=%2d of %2d, lrank=%2d of %2d]\n",
		distance,pass,mynewgroupnr,sion_gendata->grank,sion_gendata->gsize,
		comm_send_grpnum,sion_gendata->numfiles,
		comm_send_rank,comm_send_size  ));
      break;
    case _SION_BW_SCHED_NOACTION: 
      rc=sion_gendata->apidesc->create_lcg_cb(&dummycommgroup,sion_gendata->comm_data_global,
					      sion_gendata->grank,sion_gendata->gsize,
					      0,1,
					      -1,sion_gendata->numfiles);
      DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, "COMM_created dummy (d=%1d, p=%1d) [gendata: mygrp=%2d grank=%2d of %2d, group=%2d of %2d, lrank=%2d of %2d]\n",
		distance,pass,mynewgroupnr,sion_gendata->grank,sion_gendata->gsize,
		-1,sion_gendata->numfiles,0,1  ));
      break;

    default: break;
    }
  }

  /* free fields */
  if(sion_gendata->grank==0) {
    free(tasktogroup);
    free(tmpintfield1);
    free(size_per_group);
  }
  free(group_map_new_to_orig);
  free(group_map_orig_to_new);

  return(rc);
}
#undef DFUNCTION

  /* pass=1..3 */
#define DFUNCTION "_sion_buddy_bwsched"
int _sion_buddy_bwsched(int groupnr, int numgroups, int pass) {
  int res=_SION_BW_SCHED_NOACTION;
  if(numgroups%2==0) { 	/* #groups even */
    if(groupnr%2==0) {      /* groupnr even */
      if(pass==1) res=_SION_BW_SCHED_ACTIONA;
      if(pass==2) res=_SION_BW_SCHED_ACTIONB;
    } else {              /* groupnr even */
      if(pass==1) res=_SION_BW_SCHED_ACTIONB;
      if(pass==2) res=_SION_BW_SCHED_ACTIONA;
    }
  } else {
    if(groupnr%2==0) {      /* groupnr even */
      if((pass==1) && (groupnr != (numgroups-1))) res=_SION_BW_SCHED_ACTIONA;
      if((pass==2) && (groupnr != 0))             res=_SION_BW_SCHED_ACTIONB;
      if((pass==3) && (groupnr == 0))             res=_SION_BW_SCHED_ACTIONB;
      if((pass==3) && (groupnr == (numgroups-1))) res=_SION_BW_SCHED_ACTIONA;
    } else {              /* groupnr even */
      if(pass==1) res=_SION_BW_SCHED_ACTIONB;
      if(pass==2) res=_SION_BW_SCHED_ACTIONA;
    }
  }
  return(res);
}
#undef DFUNCTION

/*!\brief collect mapping information on rank 0 of first file, mapping=NULL for all others
 *
 * @return 1 if successful
 */
#define DFUNCTION "_sion_generic_collect_mapping"
int _sion_generic_collect_mapping_buddy(  _sion_generic_buddy   *buddyptr,
					  _sion_generic_gendata *sion_gendata,
					  int                    *mapping_size,
					  sion_int32           **mapping) {
  int rc=SION_SUCCESS;
  int t;
  _sion_generic_apidesc *sion_apidesc;
  sion_int32 lpos[2], *receivemap=NULL, iamreceiver, receiver = -1;

  sion_apidesc=sion_gendata->apidesc;

  *mapping = NULL;  *mapping_size = 0;
 
    
  /* mapping data will be collected by master of first physical file */
  if((buddyptr->buddy_coll.groupnum==0) && (buddyptr->buddy_coll.rank==0)) {
    /* allocate data */
    *mapping_size=sion_gendata->gsize;
    *mapping = (sion_int32 *) malloc(*mapping_size * 2 * sizeof(sion_int32));
    if (*mapping == NULL) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_collect_mapping_buddy: Cannot allocate memory for mapping"));
    }
  } 
      
  /* gather info about send about global rank of master of first file on grank 0 */
  if(sion_gendata->grank==0) {
    receivemap = (sion_int32 *) malloc(sion_gendata->gsize * sizeof(sion_int32));
    if (receivemap == NULL) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_collect_mapping_buddy: Cannot allocate memory for receivemap"));
    }
  }
    
  if((buddyptr->buddy_coll.filenum==0) && (buddyptr->buddy_coll.rank==0)) iamreceiver=sion_gendata->grank;
  else                                                                    iamreceiver=-1;
  sion_apidesc->gatherr_cb(&iamreceiver, receivemap, sion_gendata->comm_data_global, _SION_INT32, 1, 0);
  if(sion_gendata->grank==0) {
    for(t=0;t<sion_gendata->gsize;t++) {
      if(receivemap[t]>=0) {
	receiver=receivemap[t];
	break;
      }
    }
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "receiver of mapping grank=%d\n", receiver));
  }
  sion_apidesc->bcastr_cb(&receiver, sion_gendata->comm_data_global, _SION_INT32, 1, 0);
    
  /* receive global rank of master of first file on grank 0 */
  lpos[0] = buddyptr->buddy_send.filenum;
  lpos[1] = buddyptr->buddy_send.rank;
  sion_apidesc->gatherr_cb(&lpos, *mapping, sion_gendata->comm_data_global, _SION_INT32, 2, receiver);

  if(receivemap!=NULL) free(receivemap);
    
  return(rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_generic_buddy_get_and_distribute_info_from_file"
int _sion_generic_buddy_get_and_distribute_info_from_file(  _sion_generic_gendata *sion_gendata, char *fname, int root,
							    sion_int32 *filenumber, sion_int32 *numfiles, 
							    sion_int32 *lrank, sion_int32 *lsize) {
  
  int       sid = -1,  ntasks, nfiles, t;
  int       rc = SION_SUCCESS;
  FILE     *fileptr;
  sion_int32 fsblksize;
  int      *tasksinfile = NULL;
  int         mapping_size = -1;
  sion_int32 *mapping = NULL;
  sion_int32 lpos[2];
  _sion_generic_apidesc *sion_apidesc;

  sion_apidesc=sion_gendata->apidesc;
  
  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "entering function sion_gendata->grank=%d\n",sion_gendata->grank));
  
  if(sion_gendata->grank == root) {
    /* open and get mapping of sion file */
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "before open\n"));
    sid=_sion_open_read(fname,_SION_FMODE_READ|_SION_FMODE_ANSI,_SION_READ_MASTER_ONLY_OF_MULTI_FILES,
			&ntasks,&nfiles,NULL,&fsblksize,NULL,&fileptr);
    if(sid>=0) {
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "after open\n"));
      rc=sion_get_mapping(sid,&mapping_size,&mapping,numfiles);
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "sion file %d files\n", *numfiles));
    } else {
      *numfiles=-1;
    }
  }


  /* each task has to know if more than one file was used in sion file */
  sion_apidesc->bcastr_cb(numfiles, sion_gendata->comm_data_global, _SION_INT32, 1, root);
  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "%s: numfiles=%d\n",fname,*numfiles));

  if((sion_gendata->grank == root) && (*numfiles>1)) {
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "%s: mapping_size=%d sion_gendata->gsize=%d\n",fname,*numfiles,mapping_size,sion_gendata->gsize));
    if(mapping_size!=sion_gendata->gsize) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
			      "_sion_generic_buddy_get_and_distribute_info_from_file: Incorrect sum of ntasks of files %d <> %d\n", 
			      mapping_size, sion_gendata->gsize));
    }
  }

  if(*numfiles<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
			    "_sion_generic_buddy_get_and_distribute_info_from_file: could not get numfiles from sion file\n"));
  }

  if(*numfiles>1) {
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "before scatter\n"));
    if(sion_gendata->grank==root) {
      for(t=0;t<mapping_size;t++) {
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "  %d -> (%d,%d)\n",t,mapping[t*2],mapping[t*2+1]));
      }
    }

    /* scatter mapping to all tasks */
    sion_apidesc->scatterr_cb(mapping, lpos, sion_gendata->comm_data_global, _SION_INT32, 2, root);
    *filenumber=lpos[0];
    *lrank     =lpos[1];
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "after scatter filenum+lrank (%d,%d)\n",*filenumber,*lrank));

    /* compute and scatter number of tasks in each file  */
    if(sion_gendata->grank==root) {
      tasksinfile = (int *) malloc(*numfiles * sizeof(int));
      if (tasksinfile == NULL) {
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_get_and_distribute_info_from_file: Cannot allocate memory for tasksinfile counter vector"));
      }
      for(t=0;t<*numfiles;t++) tasksinfile[t]=0;                                 /* init counter */
      for(t=0;t<mapping_size;t++)  tasksinfile[ mapping[t*2] ]++;	        /* count tasks in file  */
      for(t=0;t<mapping_size;t++)  mapping[t*2+1]=tasksinfile[ mapping[t*2] ];  /* set 2nd value of mapping to lsize  */
    }
    sion_apidesc->scatterr_cb(mapping, lpos, sion_gendata->comm_data_global, _SION_INT32, 2, root);
    *lsize     =lpos[1];
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "after scatter lsize (%d, %d of %d)\n",*filenumber, *lrank, *lsize));

    if(sion_gendata->grank==root) {
      if(tasksinfile) free(tasksinfile);
    }
    /* WARNING: mapping file of sion file is now destroyed and should not be used until close  */

  } else {
    *filenumber=0;
    *lrank     = sion_gendata->grank;
    *lsize     = sion_gendata->gsize;
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "only one file -> filenumber=%d lRank=%d\n",*filenumber,*lrank));
  }

  if(sion_gendata->grank == root) {
    /* frees also mapping vector */
    if (sid>=0) _sion_close_sid(sid);
  }

  return(rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_generic_buddy_get_and_distribute_info_from_one_file"
int _sion_generic_buddy_get_and_distribute_info_from_one_file(  _sion_generic_gendata *sion_gendata, char *fname, int root,
								sion_int32 *filenumber, sion_int32 *numfiles, 
								sion_int32 *lrank, sion_int32 *lsize) {
  
  int          rc = SION_SUCCESS;
  int         t, mapping_size = 0, grank, *mapping = NULL;
  sion_int32  file_filenumber,file_numfiles,file_lrank,file_lsize;
  _sion_filedesc *sion_filedesc = NULL;
  _sion_fileptr  *sion_fileptr; 
  _sion_generic_apidesc *sion_apidesc;

  sion_apidesc=sion_gendata->apidesc;
  
  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "entering function sion_gendata->grank=%d\n",sion_gendata->grank));
  
  if(sion_gendata->grank == root) {
    /* open and get globalranks of sion file */
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "before open\n"));
    /* allocate and initialise internal data structure with default values (NULL and -1) */
    sion_filedesc = _sion_alloc_filedesc();
    if (sion_filedesc == NULL) {
      return(_sion_errorprint(SION_ID_UNDEF,SION_ID_UNDEF,
			      "sion_open: cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...\n", 
			      (unsigned long) sizeof(sion_filedesc)));
    }
    _sion_init_filedesc(sion_filedesc);
    
    /* open file */
    sion_fileptr = _sion_file_open(fname,SION_FILE_FLAG_POSIX|SION_FILE_FLAG_READ,0);
    if (!sion_fileptr) {
      return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot open %s for reading, aborting ...\n", fname));
    }
    sion_filedesc->fileptr = sion_fileptr;

    /* read part of header which does not depend on ntasks */
    rc = _sion_read_header_fix_part(sion_filedesc);
    if (rc!=SION_SUCCESS) {
      return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot read header from file %s, aborting ...\n", fname));
    }
    sion_filedesc->rank  = 0;
    sion_filedesc->state = SION_FILESTATE_SEROPEN;
    sion_filedesc->mode  = SION_FILEMODE_READ;
    
    /* memory allocation for internal fields */
    _sion_alloc_filedesc_arrays(sion_filedesc);

    /* read part of header which depends on ntasks */
    rc = _sion_read_header_var_part(sion_filedesc);
    if (rc!=SION_SUCCESS) {
      return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot read header (var part) from file %s, aborting ...\n", fname));
    }
    
    file_numfiles=sion_filedesc->nfiles;
    file_filenumber=sion_filedesc->filenumber;
    file_lsize=sion_filedesc->ntasks;
    
    /* allocate mapping vector */
    mapping_size=sion_gendata->gsize;
    mapping = (sion_int32 *) malloc(mapping_size * 2 * sizeof(sion_int32));
    if (mapping == NULL) {
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_buddy_get_and_distribute_info_from_one_file: cannot allocate temporary memory of size %lu (mapping), aborting ...\n",
				(unsigned long) mapping_size * 2 * sizeof(sion_int32)));
    }

  }


  /* each task has to know some number about local file */
  sion_apidesc->bcastr_cb(&file_numfiles, sion_gendata->comm_data_global, _SION_INT32, 1, root);
  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "%s: numfiles=%d\n",fname,file_numfiles));
  sion_apidesc->bcastr_cb(&file_filenumber, sion_gendata->comm_data_global, _SION_INT32, 1, root);
  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "%s: filenumber=%d\n",fname,file_filenumber));
  sion_apidesc->bcastr_cb(&file_lsize, sion_gendata->comm_data_global, _SION_INT32, 1, root);
  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "%s: file_lsize=%d\n",fname,file_lsize));


  if(file_numfiles!=*numfiles) {
    _SION_SAFE_FREE(mapping, NULL);
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
			    "_sion_generic_buddy_get_and_distribute_info_from_one_file: Incorrect number of files %d <> %d\n", 
			    file_numfiles,*numfiles));
  }

  /* init mapping vector lrank */
  DPRINTFP((1, DFUNCTION, sion_gendata->grank, "before scatter\n"));
  if(sion_gendata->grank==root) {
    for(t=0;t<mapping_size;t++) mapping[t]=-1;
    for(t=0;t<file_lsize;t++) {
      grank=sion_filedesc->all_globalranks[t];
      if(grank>=0) {
	mapping[grank]=t;
	DPRINTFP((1, DFUNCTION, sion_gendata->grank, "  file=%s mapping[%d] -> (%d)\n",fname,grank,mapping[grank]));
      }
    }
  }

  /* scatter mapping to all tasks */
  sion_apidesc->scatterr_cb(mapping, &file_lrank, sion_gendata->comm_data_global, _SION_INT32, 1, root);
  if(file_lrank!=-1) {
    *filenumber=file_filenumber;
    *lrank     =file_lrank;
    *lsize     =file_lsize;
    DPRINTFP((1, DFUNCTION, sion_gendata->grank, "after scatter filenum=%d lrank=%d lsize=%d\n",*filenumber,*lrank,*lsize));
  }

  if(sion_gendata->grank == root) {
    /* close current file */
    _sion_free_filedesc_arrays(sion_filedesc);
    _sion_file_close(sion_filedesc->fileptr);
    sion_filedesc->fileptr=NULL;
    free(mapping);
  }

  return(rc);
}
#undef DFUNCTION
