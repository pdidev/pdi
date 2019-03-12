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
* File: include/flowvr/ipc/mpsignal.h                             *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_IPC_MPSIGNAL_H
#define FLOWVR_IPC_MPSIGNAL_H

#include "flowvr/config.h"

#include "flowvr/ipc/mplock.h"
#include <cerrno>

#if defined(FLOWVR_USE_MPTHREAD)
// Multi-process aware pthread

namespace flowvr
{

namespace ipc
{

/// Multi-process signal.
class MPSignal
{
public:
  flowvr::mem::MPString name; ///< only used for debugging

  //TODO: Error checking

  /// Initialization.
#ifdef FLOWVR_IPC_DEBUG
  void init(const char* myname)
  {
    name = myname;
    std::cout<<"MPSignal("<<(std::string)name<<").init()"<<std::endl;
#else
  void init(const char* /*myname*/=NULL)
  {
#endif
    //MPLock::init();
    pthread_condattr_t cattr;
    pthread_condattr_init(&cattr);
    pthread_condattr_setpshared(&cattr,PTHREAD_PROCESS_SHARED);
    pthread_cond_init(&cond,&cattr);
    pthread_condattr_destroy(&cattr);
  }

  /// Destruction
  void close()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MPSignal("<<(std::string)name<<").close()"<<std::endl;
#endif
    pthread_cond_destroy(&cond);
    //MPLock::close();
  }

  /// Wait for someone to signal us (the caller already  hold the lock).
  void wait(MPLock& lock)
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<">MPSignal("<<(std::string)name<<").wait("<<(std::string)lock.name<<")"<<std::endl;
#endif
    pthread_cond_wait(&cond,&lock.mutex);
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"<MPSignal("<<(std::string)name<<").wait("<<(std::string)lock.name<<")"<<std::endl;
#endif
  }

  /// Wait for someone to signal us (the caller already  hold the lock) with the duration bounded by abstime. Return true if time is out.
  bool timedwait(MPLock& lock, const struct timespec* abstime)
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<">MPSignal("<<(std::string)name<<").timedwait("<<(std::string)lock.name<<")"<<std::endl;
#endif
    bool timeout = (pthread_cond_timedwait(&cond,&lock.mutex,abstime) == ETIMEDOUT);
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"<MPSignal("<<(std::string)name<<").timedwait("<<(std::string)lock.name<<")"<<std::endl;
#endif
    return timeout;
  }

  /// Wake one process waiting.
  void notify()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MPSignal("<<(std::string)name<<").notify()"<<std::endl;
#endif
    pthread_cond_signal(&cond);
  }

  /// Wake all processes waiting.
  void notifyAll()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MPSignal("<<(std::string)name<<").notifyAll()"<<std::endl;
#endif
    pthread_cond_broadcast(&cond);
  }

protected:
  pthread_cond_t cond;

}; // class MPSignal

#else
// fall back to active-loop lock

#include <unistd.h> // for usleep
#include <sys/time.h> // for usleep

namespace flowvr
{

namespace ipc
{

/// Multi-process signal.
///
/// This implementation is hand-written using two atomic counters.
/// nwaiting counts the number of time the wait method has been called.
/// nsignaled counts the number of notifications sent.
/// An active loop is used to wait for notifications.
class MPSignal
{
public:

  //TODO: Error checking
  flowvr::mem::MPString name; ///< only used for debugging

  /// Initialization.
#ifdef FLOWVR_IPC_DEBUG
  void init(const char* myname)
  {
    name = myname;
    std::cout<<"MPSignal("<<(std::string)name<<").init()"<<std::endl;
#else
  void init(const char* name=NULL)
  {
#endif
    nwaiting = 0;
    nsignaled = 0;
  }

  /// Destruction
  void close()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MPSignal("<<(std::string)name<<").close()"<<std::endl;
#endif
    nsignaled=-1000000;
  }

  /// Wait for someone to signal us (the caller already  hold the lock).
  void wait(MPLock& lock)
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<">MPSignal("<<(std::string)name<<").wait("<<(std::string)lock.name<<"): nwaiting="<<(int)nwaiting<<" nsignaled="<<(int)nsignaled<<std::endl;
#endif
    int tries = 0;
    // the lock protect us from corruptions of the nwaiting counter
    int num = nwaiting;
    nwaiting++;
    lock.unlock(); // free the lock before waiting
    while(nsignaled<=num)
    {
      tries++;
      if (tries<100)
#ifdef __APPLE__
	pthread_yield_np();
#else
	pthread_yield();
#endif
      else
	usleep(10000);
    }
    lock.lock(); // acquire the lock again
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"<MPSignal("<<(std::string)name<<").wait("<<(std::string)lock.name<<") after "<<tries<<" tries"<<std::endl;
#endif
  }

  /// Wait for someone to signal us (the caller already  hold the lock) with the duration bounded by abstime.
  bool timedwait(MPLock& lock, const struct timespec* abstime)
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<">MPSignal("<<(std::string)name<<").timedwait("<<(std::string)lock.name<<")"<<std::endl;
#endif
    int tries = 0;
    // the lock protect us from corruptions of the nwaiting counter
    int num = nwaiting;
    nwaiting++;
    lock.unlock(); // free the lock before waiting
    struct timeval abs;
    abs.tv_sec = abstime->tv_sec;
    abs.tv_usec = abstime->tv_nsec/1000;
    struct timeval now;
    gettimeofday(&now,NULL);
    while((now.tv_sec<abs.tv_sec || (now.tv_sec==abs.tv_sec && now.tv_usec < abs.tv_usec)) && nsignaled<=num)
    {
      tries++;
      if (tries<100)
#ifdef __APPLE__
        pthread_yield_np();
#else
        pthread_yield();
#endif
      else
	usleep(10000);
      gettimeofday(&now,NULL);
    }
    lock.lock(); // acquire the lock again
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"<MPSignal("<<(std::string)name<<").timedwait("<<(std::string)lock.name<<")"<<std::endl;
#endif
    if (nsignaled<=num) // timeout
    {
      ++nsignaled; // fake a signal
      return true;
    }
    return false;
  }

  /// Wake one process waiting.
  void notify()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MPSignal("<<(std::string)name<<").notify(): nwaiting="<<(int)nwaiting<<" nsignaled="<<(int)nsignaled<<std::endl;
#endif
    nsignaled++; // free one process
  }

  /// Wake all processes waiting.
  void notifyAll()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MPSignal("<<(std::string)name<<").notifyAll(): nwaiting="<<(int)nwaiting<<" nsignaled="<<(int)nsignaled<<std::endl;
#endif
    nsignaled = nwaiting; // free them all
  }

protected:
  MPAtomicInt nwaiting; ///< number of times the wait method has been called
  MPAtomicInt nsignaled; ///< number of notifications sent

}; // class MPSignal

#endif

} // namespace ipc

} // namespace flowvr

#endif
