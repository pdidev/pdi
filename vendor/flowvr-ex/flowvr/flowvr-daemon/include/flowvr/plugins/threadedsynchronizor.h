/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Daemon and Base Plugins                     *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
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
* File: include/flowvr/plugins/synchronizor.h                     *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGINS_THREADEDSYNCHRONIZOR
#define FLOWVR_PLUGINS_THREADEDSYNCHRONIZOR

#include "synchronizor.h"
#include "flowvr/ipc/mtsignal.h"
#include <flowvr/thread.h>


namespace flowvr
{

namespace plugins
{

/// Threaded synchronizors base class.
///
/// A synchronizor does not necessary extend it but it is recommended.
  class ThreadedSynchronizor : public Synchronizor, public flowvr::Thread
{
 public:

  /// Constructor
  ThreadedSynchronizor(const std::string &objID);

  //wait until the specified messagequeue get a new message
  void wait(int mqid);
   
  //wait on a messagequeue until the message with the specified number is received
  void wait(int mqid, int msgnum); 
   
  //wait until each messagequeue have a new message
  void wait_all();
   
  //wait any new message on any messagequeue
  void wait_any();
   
  /// Overloading of NewMessageNotification, signal is needed for the threadedsynchronizor
  
  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, plugd::Dispatcher* dispatcher);
  
  virtual plugd::Class* getClass() const;
  
 protected:

  ~ThreadedSynchronizor();
  
  ipc::MTSignal global_signal;

};

extern flowvr::plugd::AbsClass<ThreadedSynchronizor> ThreadedSynchronizorClass;

} // namespace plugins

} // namespace flowvr


#endif
