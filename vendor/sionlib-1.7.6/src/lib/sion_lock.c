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
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#define sion_assert(expr, args) (assert(expr))

#include "sion.h"
#include "sion_debug.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"

#include "sion_lock.h"

/*! container storing lock functions */
struct _sion_lock_user_cb_struct {
  int lock_defined;
  int  (*user_lock)(void *);
  int  (*user_unlock)(void *);
  void *user_lock_data;
};
typedef struct _sion_lock_user_cb_struct _sion_lock_user_cb_t;
static _sion_lock_user_cb_t _sion_lock_user_cb = {0, NULL, NULL, NULL};

/* from configure */
#ifdef SION_PTHREADS
/* from Makefile */
#ifdef SION_USE_PTHREADS
#define _USE_PTHREADS
#endif
#else
#define _USE_NOT_PTHREADS
#endif

#ifdef _USE_PTHREADS
#include <pthread.h>
#define SION_FDDATA_INITIALIZER {PTHREAD_MUTEX_INITIALIZER}
#else
#define SION_FDDATA_INITIALIZER {}
#endif

#ifdef _USE_PTHREADS
static struct _sion_lockdata {
  pthread_mutex_t lock;
} sion_lockdata = SION_FDDATA_INITIALIZER;
#endif

int _sion_lock_register_lock_callbacks(int lock(void *), int unlock(void *), void *lock_data) {
  int rc = SION_SUCCESS;
  

  lock(lock_data);
  DPRINTFP((2, "_sion_lock_register_lock_callbacks", _SION_DEFAULT_RANK, "enter\n"));
  if(_sion_lock_user_cb.lock_defined==1) {
    rc=SION_NOT_SUCCESS;
  } else {
    _sion_lock_user_cb.lock_defined=1;
    _sion_lock_user_cb.user_lock=lock;
    _sion_lock_user_cb.user_unlock=unlock;
    _sion_lock_user_cb.user_lock_data=lock_data;
  }
  DPRINTFP((2, "_sion_lock_register_lock_callbacks", _SION_DEFAULT_RANK, "lock and unlock function defined, rc=%d\n",rc));
  unlock(lock_data);


  return(rc);
}

int _sion_lock_user_callbacks_defined(void) {
  int rc;
  _sion_lock();
  rc=_sion_lock_user_cb.lock_defined;
  _sion_unlock();
  DPRINTFP((2, "_sion_lock_user_callbacks_defined", _SION_DEFAULT_RANK, "check user lock cbs, rc=%d\n",rc));
  return(rc);
};


#ifdef _USE_PTHREADS
int _sion_lock(void) {
  int rc=SION_SUCCESS;
  if(_sion_lock_user_cb.lock_defined==1) {
    _sion_lock_user_cb.user_lock(_sion_lock_user_cb.user_lock_data);
  } else {
    if(pthread_mutex_lock(&sion_lockdata.lock)) {
      rc=SION_NOT_SUCCESS;
    };
  }
  return(rc);
};

int _sion_unlock(void) {
  int rc=SION_SUCCESS;
  if(_sion_lock_user_cb.lock_defined==1) {
    _sion_lock_user_cb.user_unlock(_sion_lock_user_cb.user_lock_data);
  } else {
    if(pthread_mutex_unlock(&sion_lockdata.lock)) {
      rc=SION_NOT_SUCCESS;
    }
  }
  return(rc);
}

#else 

int _sion_lock(void) {
  if(_sion_lock_user_cb.lock_defined==1) {
    _sion_lock_user_cb.user_lock(_sion_lock_user_cb.user_lock_data);
  } 
  return(SION_SUCCESS);
};
int _sion_unlock(void) {
  if(_sion_lock_user_cb.lock_defined==1) {
    _sion_lock_user_cb.user_unlock(_sion_lock_user_cb.user_lock_data);
  } 
  return(SION_SUCCESS);
};

#endif
