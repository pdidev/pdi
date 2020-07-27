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
* File: include/flowvr/ipc/mpchannel.h                            *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_IPC_MPCHANNEL_H
#define FLOWVR_IPC_MPCHANNEL_H

#include "flowvr/config.h"

#include "flowvr/common.h"

#include "flowvr/ipc/mplock.h"
#include "flowvr/ipc/mpsignal.h"

namespace flowvr
{

namespace ipc
{

/// Multi-process channel.
///
/// This class implements the logic of a communication channel. The data itself must be externally stored.
/// It is implemented using 2 counters: the number of elements read (nRead) and the number of elements sent (nSent).
/// 
class MPChannel
{
public:
  flowvr::mem::MPString name; ///< only used for debugging

  /// Initialize the channel with the specified capacity.
#ifdef FLOWVR_IPC_DEBUG
  void init(int cap,const char* myname)
  {
    name = myname;
    std::cout<<"MPChannel("<<(std::string)name<<").init("<<cap<<")"<<std::endl;
#else
  void init(int cap,const char* /*myname*/=NULL)
  {
#endif
    capacity = cap;
    nRead = 0;
    nSend = 0;
    sending = false;
#ifdef FLOWVR_IPC_DEBUG
    std::string s;
    s = std::string(myname)+".lock";
    lock.init(s.data());
    s = std::string(myname)+".sigCanRead";
    sigCanRead.init(s.data());
    s = std::string(myname)+".sigCanSend";
    sigCanSend.init(s.data());
#else
    lock.init();
    sigCanRead.init();
    sigCanSend.init();
#endif
  }

  /// Send a new message. Return the index to write to.
  int beginSend()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::string s;
    s = std::string(name)+".beginSend()";
    ipc::ScopedMPLock locker(lock,s.c_str());
#else
    ipc::ScopedMPLock locker(lock);
#endif
    // wait for other send operations to complete
    while(sending)
      sigCanRead.wait(lock);

    while (nSend>=nRead+capacity)
      sigCanSend.wait(lock);
    sending=true;
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<(std::string)name<<": Sending on slot "<<nSend%capacity<<std::endl;
#endif
    return nSend%capacity;
  }

  /// Try to send a new message. Return the index to write to, or -1 if channel is full
  int tryBeginSend()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::string s;
    s = std::string(name)+".tryBeginSend()";
    ipc::ScopedMPLock locker(lock,s.data());
#else
    ipc::ScopedMPLock locker(lock);
#endif
    if (sending || nSend>=nRead+capacity)
    {
      return -1;
    }
    else
    {
      sending=true;
#ifdef FLOWVR_IPC_DEBUG
      std::cout<<(std::string)name<<": Sending on slot "<<nSend%capacity<<std::endl;
#endif
      return nSend%capacity;
    }
  }

  /// End send operation. Possibly waiting for the reader to read the message.
  int endSend(bool waitreader=false)
  {
#ifdef FLOWVR_IPC_DEBUG
    std::string s;
    s = std::string(name)+".endSend()";
    ipc::ScopedMPLock locker(lock,s.data());
#else
    ipc::ScopedMPLock locker(lock);
#endif
    int id = nSend;
    ++nSend;
    sending = false;
    sigCanRead.notifyAll();
    if (waitreader)
    {
      while (nRead<=id)
	sigCanSend.wait(lock);
    }
    return 0;
  }

  /// Request the next message to read. Return the index to read from.
  int beginRead()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::string s;
    s = std::string(name)+".beginRead()";
    ipc::ScopedMPLock locker(lock,s.data());
#else
    ipc::ScopedMPLock locker(lock);
#endif
    while (nSend<=nRead)
      sigCanRead.wait(lock);
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<(std::string)name<<": Reading on slot "<<nRead%capacity<<std::endl;
#endif
    return nRead%capacity;
  }

  /// End read operation.
  void endRead()
  {
#ifdef FLOWVR_IPC_DEBUG
    std::string s;
    s = std::string(name)+".endRead()";
    ipc::ScopedMPLock locker(lock,s.data());
#else
    ipc::ScopedMPLock locker(lock);
#endif
    ++nRead;
    //if (nSend == nRead+capacity-1)
    sigCanSend.notifyAll();
  }

  int size() const
  {
    return capacity;
  }

protected:
  int capacity;
  int nRead;
  int nSend;
  bool sending;
  MPLock lock;
  MPSignal sigCanRead;
  MPSignal sigCanSend;

}; // class MPChannel

} // namespace ipc

} // namespace flowvr

#endif
