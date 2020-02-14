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
 * \brief Debugging output
 *
 * \author Th.Eickermann & W.Frings (January 2001)
 *
 * Print debugging info according to the following environment variables:
 * <ul>
 *   <li>   SION_DEBUG        = filename - print to "filename" or stdout if filename is empty
 *   <li>   SION_MASK         = Binary mask used to select which messages should be logged
 *   <li>   SION_DEBUG_RANK   = Specifies for which rank to print the messages
 *   <li>   SION_DEBUG_RANK1  = The same as SION_DEBUG_RANK
 *   <li>   SION_DEBUG_RANK2  = Specifies a second rank to log
 * </ul>
 *
 * The value for the mask can be calculated by adding the following values:
 * <ul>
 *   <li>   1 -> sion user function entries and exits
 *   <li>   2 -> sion internal function entries and exits
 *   <li>   8 -> high frequently called sion user function entries and exits
 *   <li>  16 -> high frequently called sion internal function entries and exits
 *   <li>  32 -> internal steps of sion internal function
 *   <li>  64 -> internal steps of sion user function
 *   <li> 128 -> timings (top level)
 *   <li> 256 -> timings (low level)
 *   <li> 512 -> print filedesc structure contents
 *   <li>2048 -> higher frequently called sion internal function (internal steps)
 * </ul>
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "sion_lock.h"

#include "sion_debug.h"
#include "sion_internal.h"

#define SIONDEBFUNCNAMELEN 50
#define MAXOMPTHREADS 300


int _sion_get_thread_num_default(void);
static int (*_sion_my_get_thread_num)(void) = _sion_get_thread_num_default;

/*! first call of dprintf? */
static int _sion_debug_first[MAXOMPTHREADS] = {1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,

					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1,
					       1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1};

/*! debug output is printed to out */
static FILE *_sion_debug_out[MAXOMPTHREADS] = {NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,

					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL,
					       NULL,NULL,NULL,NULL,NULL,  NULL,NULL,NULL,NULL,NULL};

/*! true, if debug output is desired */
static int _sion_debug_isdebug[MAXOMPTHREADS] = {0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,

						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,

						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0,
						 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0};

/*! MPI rank of process */
static int _sion_debug_myrank[MAXOMPTHREADS] = {-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,

						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,

						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1,
						-1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1, -1,-1,-1,-1,-1};

static int _sion_debug_debmask  = 1023;      /*!< mask to define the quantity of debug output */
static int _sion_debug_debrank1 = -1;       /*!< first rank for which to log debug information */
static int _sion_debug_debrank2 = -1;       /*!< second rank for which to log debug information */
static int _sion_debug_silent   = 0;        /*!< describes if debug interface should not print anythink on stdout/err */
static int _sion_debug_isinit   = 0;        /*!< describes if debug interface is initialized */
static char * _sion_debug_fn    = NULL;     /*!< contains filename set by SION_DEBUG */

#define _DEBUG_INIT_THREAD \
   int threadid=0; \
   threadid = _sion_my_get_thread_num(); \
   if(threadid>MAXOMPTHREADS) threadid=(MAXOMPTHREADS-1);


/* default thread number */
int _sion_get_thread_num_default(void) {
  return(0);
}

/* default thread number */
int _sion_debug_set_query_thread_num_function( int (*get_thread_num)(void) ) {
  _sion_lock();
  _sion_my_get_thread_num=get_thread_num;
  _sion_unlock();
  return(1);
}

/*!
 * \brief Print debugging info formating the message like printf
 *
 */
int sion_dprintf(int mask, const char *format, ...)
{
  va_list   ap;
  _DEBUG_INIT_THREAD

  if (_sion_debug_first[threadid])
    _sion_debug_init();

  if ((!_sion_debug_isdebug[threadid]) || !(mask & _sion_debug_debmask))
    return 1;

  fprintf(_sion_debug_out[threadid], "    ");

  va_start(ap, format);

  vfprintf(_sion_debug_out[threadid], format, ap);

  va_end(ap);

  return 1;
}

/*!
 * \brief Print debugging info formating the message like printf and including the name of the calling function
 *
 */
int sion_dprintfp(int mask, const char *callfunction, int rank, const char *format, ...)
{
  va_list   ap;
  char tmpfuncname[SIONDEBFUNCNAMELEN + 1];
  char spec[20];
  int       setrank = 0, norank = 0;
  _DEBUG_INIT_THREAD

  if ((_sion_debug_myrank[threadid] < 0) && (rank < 0)) {
    return (0);
  }

  /* no rank specified, used previous stored rank */
  if (rank < 0) {
    rank = _sion_debug_myrank[threadid];
    norank = 1;
  }

  /* if internal rank is not initialized used rank parameter, otherwise don't overwrite it  */
  if (_sion_debug_myrank[threadid] < 0) {
    _sion_debug_myrank[threadid] = rank;
    setrank = 1;
  }
  /*   if(setrank) */
  /*     fprintf(stderr,"WF: in sion_dprintfp: mask=%d %s rank=%d myrank=%d setrank=%d norank=%d first=%d\n",mask,callfunction,rank,myrank,setrank,norank,_sion_debug_first);  */

  if (_sion_debug_first[threadid])
    _sion_debug_init();

  /* fprintf(stderr,"WF: in sion_dprintfp: mask=%d %s rank=%d debmask=%d and=%d\n",mask,callfunction,rank,debmask,mask & debmask); */

  if ((!_sion_debug_isdebug[threadid]) || !(mask & _sion_debug_debmask))
    return 1;
  if ((_sion_debug_debrank1 >= 0) && (_sion_debug_debrank2 >= 0)) {
    if ((rank != _sion_debug_debrank1) && (rank != _sion_debug_debrank2))
      return 1;
  }
  else if (_sion_debug_debrank1 >= 0) {
    if (rank != _sion_debug_debrank1)
      return 1;
  }

  if (strlen(callfunction) > SIONDEBFUNCNAMELEN) {
    strncpy(tmpfuncname, callfunction, SIONDEBFUNCNAMELEN);
    tmpfuncname[SIONDEBFUNCNAMELEN] = '\0';
  }
  else
    strcpy(tmpfuncname, callfunction);

  sprintf(spec, "SION[%s%s%%5d][%%-%ds] ", (setrank ? "S" : " "), (norank ? "N" : " "), SIONDEBFUNCNAMELEN);
  fprintf(_sion_debug_out[threadid], spec, rank, tmpfuncname);
  if (mask >= 8)
    fprintf(_sion_debug_out[threadid], "    ");
  if (mask > 64)
    fprintf(_sion_debug_out[threadid], "    ");
  if (mask >= 128)
    fprintf(_sion_debug_out[threadid], "    ");

  va_start(ap, format);

  vfprintf(_sion_debug_out[threadid], format, ap);

  va_end(ap);

  fflush(_sion_debug_out[threadid]);

  return 1;
}

/*!
 * return the file pointer of the debug file
 */
FILE     *sion_get_dfile(void)
{
  _DEBUG_INIT_THREAD
  if (_sion_debug_first[threadid])
    _sion_debug_init();

  return _sion_debug_out[threadid];
}

/*!
 * close the debug-file
 */
void sion_dclose(void)
{
  _DEBUG_INIT_THREAD
  if (_sion_debug_out[threadid] && (_sion_debug_out[threadid] != stdout) && (_sion_debug_out[threadid] != stderr)) {
    fclose(_sion_debug_out[threadid]);
    _sion_debug_out[threadid] = NULL;
    _sion_debug_first[threadid] = 1;
  }
}

int sion_isdebug(void)
{
  _DEBUG_INIT_THREAD
  if (_sion_debug_first[threadid])
    _sion_debug_init();

  return _sion_debug_isdebug[threadid] ? _sion_debug_debmask : 0;
}


/*!
 * \brief sets debug mode.
 *
 * \param mask debug mask
 * \param filename if not NULL, output file for debug output (default is stderr)
 */
void sion_debug_on(int mask, const char *filename)
{
  char     *fname = 0;
  _DEBUG_INIT_THREAD

  if (_sion_debug_out[threadid])
    sion_dclose();              /* close previously opened logfile */

  _sion_debug_first[threadid] = 0;                    /* do not call _sion_debug_init() */

  if (filename) {
    fname = (char *) malloc((strlen(filename) + 1) * sizeof(char));
    strcpy(fname, filename);
  }

  /* open debug output file (default is stderr) */
  if (!fname || (strlen(fname) == 0) || !strcmp(fname, "stderr")) {
    _sion_debug_out[threadid] = stderr;
  }
  else if (!strcmp(fname, "stdout")) {
    _sion_debug_out[threadid] = stdout;
  }
  else if (!(_sion_debug_out[threadid] = fopen(fname, "w"))) {
    fprintf(stderr, "sion_dprintf: failed to open '%s' for writing\n", fname);
    _sion_debug_out[threadid] = stderr;
  }

#ifdef SION_DEBUG_EXTREME
  fprintf(stderr, "Warning: you are using a version of SION that is configured with -DEBUG (current debug-mask is %d\n", debmask);
#endif

  if ((_sion_debug_out[threadid] != stdout) && (_sion_debug_out[threadid] != stderr))
    fprintf(stderr, "Writing debug output to %s\n", fname);

  if (fname)
    free(fname);

  _sion_debug_debmask = mask;
  _sion_debug_isdebug[threadid] = 1;
}

void sion_debug_off(void)
{
  _DEBUG_INIT_THREAD
  _sion_debug_isdebug[threadid] = 0;
  sion_dclose();
}


/*!
 * \brief initialize the debug environment
 *
 * evaluate the debug environment variables
 * set _sion_debug_isdebug and out
 */

int _sion_debug_init(void)
{
  int rvalue = 1;
  _DEBUG_INIT_THREAD
    {
      if (_sion_debug_first[threadid]) {
	char     *filename = 0;

	_sion_debug_first[threadid] = 0;
	_sion_debug_isdebug[threadid] = 0;

	/* get info from environment only once */
	_sion_lock();
	if(_sion_debug_isinit) {
	  _sion_unlock();
	} else {
          const char *t;

	  t = _sion_getenv("SION_DEBUG_RANK");
	  if (t)
	    _sion_debug_debrank1 = atoi(t);
	
	  t = _sion_getenv("SION_DEBUG_RANK1");
	  if (t)
	    _sion_debug_debrank1 = atoi(t);
	
	  t = _sion_getenv("SION_DEBUG_RANK2");
	  if (t)
	    _sion_debug_debrank2 = atoi(t);
	
	  t = _sion_getenv("SION_DEBUG");
	  if (t) {
	    _sion_debug_fn = (char *) t;
	  }
	
	  t = _sion_getenv("SION_DEBUG_MASK");
	  if (t)
	    _sion_debug_debmask = atoi(t);
	
	  t = _sion_getenv("SION_DEBUG_SILENT");
	  if (t)
	    _sion_debug_silent = atoi(t);

	  _sion_debug_isinit=1;
	  _sion_unlock();
	}

	if (_sion_debug_fn) {
	  _sion_debug_isdebug[threadid] = 1;              /* set debug mode if SION_DEBUG ist set */
	}
	
	if ((_sion_debug_debrank1 >= 0) && (_sion_debug_debrank2 >= 0)) {
	  if ((_sion_debug_myrank[threadid] != _sion_debug_debrank1) && (_sion_debug_myrank[threadid] != _sion_debug_debrank2))
	    _sion_debug_isdebug[threadid] = 0;
	}
	else if (_sion_debug_debrank1 >= 0) {
	  if (_sion_debug_myrank[threadid] != _sion_debug_debrank1)
	    _sion_debug_isdebug[threadid] = 0;
	}
	
	if ((_sion_debug_debrank1 == -2) && (_sion_debug_debrank2 == -2)) {
	  _sion_debug_isdebug[threadid] = 1;
	}
	if (_sion_debug_isdebug[threadid]) {

	  if ((_sion_debug_fn == 0) || (strlen(_sion_debug_fn) == 0) || !strcmp(_sion_debug_fn, "stderr")) {
	    _sion_debug_out[threadid] = stderr;
	  }
	  else if (!strcmp(_sion_debug_fn, "stdout")) {
	    _sion_debug_out[threadid] = stdout;
	  }
	  else {
	    filename=malloc((strlen(_sion_debug_fn) + 1 + 10) * sizeof(char));
	    sprintf(filename, "%s.%05d", _sion_debug_fn, _sion_debug_myrank[threadid]);
	
	    if (!(_sion_debug_out[threadid] = fopen(filename, "a"))) {
	      fprintf(_sion_debug_out[threadid], "sion_dprintf: failed to open '%s' for writing\n", filename);
	      rvalue = 0;
	    }

	    if( rvalue && ((_sion_debug_out[threadid] != stdout) && (_sion_debug_out[threadid] != stderr)) && (!_sion_debug_silent) )
	      fprintf(stderr, "Writing debug output to %s\n", filename);

	  }

	  if (filename) free(filename);

	  /* print a warning (to avoid using accidentally
	     a library that is full of DPRINTFs) */
#ifdef SION_DEBUG_EXTREME
	  fprintf(stderr, "Warning: you are using a version of SION that is configured with --with-debug (current debug-mask is %d)\n", _sion_debug_debmask);
#endif

	} /* _sion_debug_isdebug[threadid] */

      } /* _sion_debug_first[threadid] */

    }
  
  return rvalue;
}
