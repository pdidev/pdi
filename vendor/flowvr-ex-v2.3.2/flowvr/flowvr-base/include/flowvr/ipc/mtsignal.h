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
* File: include/flowvr/ipc/mtsignal.h                             *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_IPC_MTSIGNAL_H
#define FLOWVR_IPC_MTSIGNAL_H

#include <flowvr/ipc/mtlock.h>

#include <errno.h>

namespace flowvr
{

namespace ipc
{

/// Multi-thread signal
class MTSignal
{
public:

  //TODO: Error checking

  MTSignal(const char* myname
#ifndef FLOWVR_IPC_DEBUG
	 = NULL
#endif
	 )
  {
    init(myname);
  }

  ~MTSignal()
  {
    close();
  }

  /// Initialization.
#ifdef FLOWVR_IPC_DEBUG
  std::string name;
  void init(const char* myname)
  {
    name = myname;
    std::cout<<"MTSignal("<<(std::string)name<<").init()"<<std::endl;
#else
  void init(const char* name=NULL)
  {
#endif
    pthread_cond_init(&cond,NULL);
  }

  /// Destruction
  void close()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MTSignal("<<(std::string)name<<").close()"<<std::endl;
#endif
    pthread_cond_destroy(&cond);
  }

  /// Wait for someone to signal us (the caller already  hold the lock).
  void wait(MTLock& lock)
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<">MTSignal("<<(std::string)name<<").wait("<<(std::string)lock.name<<")"<<std::endl;
#endif
    pthread_cond_wait(&cond,&lock.mutex);
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"<MTSignal("<<(std::string)name<<").wait("<<(std::string)lock.name<<")"<<std::endl;
#endif
  }

  /// Wait for someone to signal us (the caller already  hold the lock) with the duration bounded by abstime. Return true if time is out.
  bool timedwait(MTLock& lock, const struct timespec* abstime)
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<">MTSignal("<<(std::string)name<<").timedwait("<<(std::string)lock.name<<")"<<std::endl;
#endif
    bool timeout = (pthread_cond_timedwait(&cond,&lock.mutex,abstime) == ETIMEDOUT);
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"<MTSignal("<<(std::string)name<<").timedwait("<<(std::string)lock.name<<")"<<std::endl;
#endif
    return timeout;
  }

  /// Wake one process waiting.
  void notify()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MTSignal("<<(std::string)name<<").notify()"<<std::endl;
#endif
    pthread_cond_signal(&cond);
  }

  /// Wake all processes waiting.
  void notifyAll()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"MTSignal("<<(std::string)name<<").notifyAll()"<<std::endl;
#endif
    pthread_cond_broadcast(&cond);
  }

protected:
  pthread_cond_t cond;

}; // class MTSignal

} // namespace ipc

} // namespace flowvr

#endif
