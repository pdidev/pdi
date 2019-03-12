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
* File: include/flowvr/ipc/mtlock.h                               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_IPC_MTLOCK_H
#define FLOWVR_IPC_MTLOCK_H

#include "flowvr/ipc/locker.h"
#ifdef FLOWVR_IPC_DEBUG
#include "flowvr/mpdata.h"
#endif

#include <pthread.h>
#include <iostream>

namespace flowvr
{

namespace ipc
{

class MTSignal;

/// Multi-thread (simple) lock.
class MTLock
{
public:

  MTLock(const char* myname
#ifndef FLOWVR_IPC_DEBUG
	 = NULL
#endif
	 )
  {
    init(myname);
  }

  ~MTLock()
  {
    close();
  }

  /// Initialization.
#ifdef FLOWVR_IPC_DEBUG
  std::string name;
  void init(const char* myname)
  {
    if (myname!=NULL) name = myname;
    std::cout<<">MTLock("<<(std::string)name<<").init()"<<std::endl;
#else
  void init(const char* /*myname*/=NULL)
  {
#endif
    pthread_mutex_init(&mutex,NULL);
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"<MTLock("<<(std::string)name<<").init()"<<std::endl;
#endif
  }

  /// Destruction.
  void close()
  {
#ifdef FLOWVR_IPC_DEBUG
    if (!name.empty()) std::cout<<"MTLock("<<(std::string)name<<").close()"<<std::endl;
#endif
    pthread_mutex_destroy(&mutex);
  }

  /// Acquire the lock.
#ifdef FLOWVR_IPC_DEBUG
  void lock(const char* by=NULL)
  {
    if (!name.empty())
    {
      std::cout<<">MTLock("<<(std::string)name<<").lock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl;
    }
#else
  void lock(const char* /*by*/=NULL)
  {
#endif
    pthread_mutex_lock(&mutex);
#ifdef FLOWVR_IPC_DEBUG
    if (!name.empty()) std::cout<<"<MTLock("<<(std::string)name<<").lock()"<<std::endl;
#endif
  }

  /// Release the lock.
#ifdef FLOWVR_IPC_DEBUG
  void unlock(const char* by=NULL)
  {
    if (!name.empty()) 
    {
      std::cout<<">MTLock("<<(std::string)name<<").unlock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl;
    }
#else
  void unlock(const char* /*by*/=NULL)
  {
#endif
    pthread_mutex_unlock(&mutex);
#ifdef FLOWVR_IPC_DEBUG
    if (!name.empty()) std::cout<<"<MTLock("<<(std::string)name<<").unlock()"<<std::endl;
#endif
  }

  bool trylock() {
      return ! pthread_mutex_trylock( &mutex );
  }

protected:
  pthread_mutex_t mutex;
  friend class MTSignal;
}; // class MTLock

typedef Scoped<MTLock> ScopedMTLock;

} // namespace ipc

} // namespace flowvr

#endif
