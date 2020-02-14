/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
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

#include "sion_buddy_common.h"


#define STR_PRT(X) case X: return # X
char* _sion_buddy_role_to_str (unsigned int flag) {
  switch (flag) {
    STR_PRT(SION_ROLE_NONE);
    STR_PRT(SION_ROLE_COLLECTOR);
    STR_PRT(SION_ROLE_WRITER);
    STR_PRT(SION_ROLE_SENDER);
    STR_PRT(SION_ROLE_NOWRITER);
    STR_PRT(SION_ROLE_READER);
    STR_PRT(SION_ROLE_NOREADER);
    STR_PRT(SION_ROLE_COLLECTOR_READER);
  default: return("undef");
  }	           
  return "";
}

#define DFUNCTION "sion_get_io_info_buddy"
/*   return list of filenames created and the size of their payload  */
sion_io_stat_t* sion_get_io_info_buddy(int sid, int roles, int flag) {
  int loop;
  _sion_filedesc *sion_filedesc;
  _sion_filedesc *p_sion_filedesc;
  sion_io_stat_t *p;

  int     p_sid, i, j, b, p_nf, do_count_size;
  _sion_generic_buddy *buddies, *buddyptr;
  
  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid);
    return(NULL);
  }

  buddies=sion_filedesc->buddies;

  /* determine number of files */
  p_nf= 0;

  /* run twice: first: count entries, second: store entries */
  for(loop=0; loop<2; loop++) {

    if(loop==1) {
      p=_sion_alloc_io_info(p_nf);
      if(p==NULL) {
	_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_get_io_info_buddy: cannot allocate io info data structure");
	return(NULL);
      }
      p->nfiles=p_nf;
    }
    


    if (sion_filedesc->mode == SION_FILEMODE_WRITE) {
      int c=0;
      int collector=0;
    
      /* original file */
      if(roles&SION_ROLE_WRITER) {
      
	if(loop==0) {
	  p_nf++;
	} else {
	  p->names[c]= strdup(sion_filedesc->fname);
	  p->roles[c]=SION_ROLE_WRITER;
	  /* size */
	  DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "BUDDY WRITE start sizes\n"));
	  _sion_print_filedesc(sion_filedesc,1,DFUNCTION,_SION_DEBUG_PRINT_ALL);
	  p->sizes[c]=0;
	  for(j=0;j<=sion_filedesc->lastchunknr;j++) {
	    p->sizes[c]+=sion_filedesc->blocksizes[j];
	  }
	  DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "BUDDY WRITE end sizes --> %d\n", p->sizes[c]));
	  c++;
	}
      }

      /* buddies */
      for(b=0;b<sion_filedesc->buddylevel;b++)  {
	buddyptr=&buddies[b];
	do_count_size=0;

	for(i=0;i<2;i++) {
	  if(i==0) p_sid=buddyptr->buddy_send.sid;
	  if(i==1) p_sid=buddyptr->buddy_coll.sid;

	  if ((p_sid<0) || (_sion_vcdtype(p_sid) != SION_FILEDESCRIPTOR)
	      || !(p_sion_filedesc = _sion_vcdtovcon(p_sid))) {
	    _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid p_sion_filedesc, aborting %d ...\n", p_sid);
	    return(NULL);
	  }
	  /* filename */

	  /* role */
	  if(i==0) {
	    if(roles&SION_ROLE_SENDER) {
	      if(loop==0) { p_nf++; } 
	      else {
		p->names[c]= strdup(p_sion_filedesc->fname);
		p->roles[c]=SION_ROLE_SENDER;
		do_count_size=1;
	      }
	    }
	  } /* i==0 */

	  if(i==1) {
	    p_sid=buddyptr->buddy_coll.sid;
	    if(p_sion_filedesc->rank == collector) {

	      if(roles&SION_ROLE_COLLECTOR) {
		if(loop==0) {p_nf++; } 
		else {
		  p->names[c]= strdup(p_sion_filedesc->fname);
		  p->roles[c]=SION_ROLE_COLLECTOR;
		  do_count_size=1;
		}
	      }

	    } else { 		/* not collector */

	      if(roles&SION_ROLE_NOWRITER) {
		if(loop==0) { p_nf++; } 
		else {
		  p->names[c]= strdup(p_sion_filedesc->fname);
		  p->roles[c]=SION_ROLE_NOWRITER;
		  do_count_size=1;
		}
	      }

	    }

	  } /* i==1 */

	  /* size */
	  if(do_count_size) {
	    DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "BUDDY WRITE b=%d i=%d start sizes\n", b, i));
	    _sion_print_filedesc(p_sion_filedesc,1,DFUNCTION,_SION_DEBUG_PRINT_ALL);
	    p->sizes[c]=0;
	    for(j=0;j<=p_sion_filedesc->lastchunknr;j++) {
	      p->sizes[c]+=p_sion_filedesc->blocksizes[j];
	    }
	    DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "BUDDY WRITE b=%d i=%d end sizes --> %d\n", b, i,p->sizes[c]));

	    c++;
	  }


	}	/* for i (buddy actions) */
      
      } /* for b (buddy level) */

    } else {   /* READ */

    }
  } /* for loop */
  
  return (p);
}
#undef DFUNCTION
