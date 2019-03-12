/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                         Base Libraries                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490). ALL RIGHTS RESERVED.                                *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/flowvr/ipc/mplock.h                               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_IPC_MPLOCK_H
#define FLOWVR_IPC_MPLOCK_H

#include "flowvr/config.h"

#include "flowvr/ipc/locker.h"
#include "flowvr/mem/mpdata.h"

#if defined(FLOWVR_USE_MPTHREAD)
// Multi-process aware pthread

#include <pthread.h>
#include <iostream>

namespace flowvr
{

namespace ipc
{

class MPSignal;

/// Multi-process (simple) lock
class MPLock
{
public:
  flowvr::mem::MPString name; ///< only used for debugging

  /// Initialization.
#ifdef FLOWVR_IPC_DEBUG
  void init(const char* myname)
  {
    if (myname!=NULL) name = myname;
    std::cout<<"MPLock("<<(std::string)name<<").init()"<<std::endl;
#else
  void init(const char* /*myname*/=NULL)
  {
#endif
    pthread_mutexattr_t mattr;
    pthread_mutexattr_init(&mattr);
    pthread_mutexattr_setpshared(&mattr,PTHREAD_PROCESS_SHARED);
    pthread_mutexattr_settype(&mattr,PTHREAD_MUTEX_NORMAL);
    pthread_mutex_init(&mutex,&mattr);
    pthread_mutexattr_destroy(&mattr);
  }

  /// Destruction.
  void close()
  {
#ifdef FLOWVR_IPC_DEBUG
    if (name.valid()) std::cout<<"MPLock("<<(std::string)name<<").close()"<<std::endl;
#endif
    pthread_mutex_destroy(&mutex);
  }

  /// Acquire the lock.
#ifdef FLOWVR_IPC_DEBUG
  void lock(const char* by=NULL)
  {
    if (name.valid())
    {
      std::cout<<">MPLock("<<(std::string)name<<").lock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl;
    }
#else
  void lock(const char* /*by*/=NULL)
  {
#endif
    pthread_mutex_lock(&mutex);
#ifdef FLOWVR_IPC_DEBUG
    if (name.valid()) std::cout<<"<MPLock("<<(std::string)name<<").lock()"<<std::endl;
#endif
  }

  /// Release the lock.
#ifdef FLOWVR_IPC_DEBUG
  void unlock(const char* by=NULL)
  {
    if (name.valid()) 
    {
      std::cout<<">MPLock("<<(std::string)name<<").unlock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl;
    }
#else
  void unlock(const char* /*by*/=NULL)
  {
#endif
    pthread_mutex_unlock(&mutex);
#ifdef FLOWVR_IPC_DEBUG
    if (name.valid()) std::cout<<"<MPLock("<<(std::string)name<<").unlock()"<<std::endl;
#endif
  }

protected:
  pthread_mutex_t mutex;
  friend class MPSignal;
}; // class MPLock

#else
// fall back to active-loop lock

#include "flowvr/ipc/atomic.h"
#include <pthread.h>
#include <iostream>

namespace flowvr
{

namespace ipc
{

class MPSignal;

/// Multi-process (simple) lock
class MPLock
{
public:
  flowvr::mem::MPString name; ///< only used for debugging

  /// Initialization.
#ifdef FLOWVR_IPC_DEBUG
  void init(const char* myname)
  {
    if (myname!=NULL) name = myname;
    std::cout<<"MPLock("<<(std::string)name<<").init()"<<std::endl;
#else
  void init(const char* myname=NULL)
  {
#endif
    avail = 1;
  }

  /// Destruction.
  void close()
  {
#ifdef FLOWVR_IPC_DEBUG
    if (name.valid()) std::cout<<"MPLock("<<(std::string)name<<").close()"<<std::endl;
#endif
    avail = -1000000;
  }

  /// Acquire the lock.
#ifdef FLOWVR_IPC_DEBUG
  void lock(const char* by=NULL)
  {
    if (name.valid())
    {
      std::cout<<">MPLock("<<(std::string)name<<").lock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl;
    }
    int tries = 0;
#else
  void lock(const char* /*by*/=NULL)
  {
#endif
    
    while (avail.add_and_test_neg(-1))
    { // not available
      avail.inc(); // undo the dec
#ifdef FLOWVR_IPC_DEBUG
      tries++;
#endif
#ifdef __APPLE__
      pthread_yield_np();
#else
      pthread_yield(); // and give-up
#endif
    }
    // we successfully acquired the lock
#ifdef FLOWVR_IPC_DEBUG
    if (name.valid()) std::cout<<"<MPLock("<<(std::string)name<<").lock() after "<<tries<<" tries"<<std::endl;
#endif
  }

  /// Release the lock.
#ifdef FLOWVR_IPC_DEBUG
  void unlock(const char* by=NULL)
  {
    if (name.valid()) 
    {
      std::cout<<">MPUnlock("<<(std::string)name<<").unlock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl; 
    }
#else
  void unlock(const char* /*by*/=NULL)
  {
#endif
    avail.inc(); // undo the dec
#ifdef FLOWVR_IPC_DEBUG
    if (name.valid()) std::cout<<"<MPUnlock("<<(std::string)name<<").unlock()"<<std::endl;
#endif
  }

protected:
  MPAtomicInt avail; ///< Number of available slots // negated and minus 1 (because we increment its value and test if still negative to know if we have the lock)
  friend class MPSignal;
}; // class MPLock

#endif

typedef Scoped<MPLock> ScopedMPLock;

} // namespace ipc

} // namespace flowvr

#endif
