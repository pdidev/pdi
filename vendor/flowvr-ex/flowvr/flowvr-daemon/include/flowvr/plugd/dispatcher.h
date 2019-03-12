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
* File: include/flowvr/plugd/dispatcher.h                         *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGD_DISPATCHER_H
#define FLOWVR_PLUGD_DISPATCHER_H

#include "flowvr/message.h"
#include "flowvr/ipc/atomic.h"
#include "flowvr/ipc/mtlock.h"

namespace flowvr
{

namespace plugd
{

class RoutingTable;
class ActionHandler;
class Action;

class Dispatcher
{
 public:
  virtual ~Dispatcher() {}

  /// Process a new message.
  virtual void process(const Message& msg, const std::vector<Action*>* actionvector)=0;

  /// Process a new message.
  virtual void process(const Message& msg)=0;

  /// Generate a dispatcher for a new thread.
  /// Depending whether the dispatcher implementation uses a centralized
  /// dispatching thread or a thread-local queue, this might create a new
  /// Dispatcher, or simply return this.
  virtual Dispatcher* threadCopy()=0;

  /// Get the routing table.
  virtual RoutingTable* getRoutingTable()=0;

  /// Signal the the dispatcher is not used anymore.
  virtual void close()=0;


  /// Process a locally-generated control message.
  ///
  /// This method should be used by any object sending a news command or by a controller's regulator.
  virtual void processControlMessage(const Message& msg)=0;

  /// Process a remotely-generated control message.
  ///
  /// This method should be used by any network handler receiving a control message.
  virtual void processRemoteControlMessage(const Message& msg)=0;

  /// Set the ActionHandler for locally-generated control messages.
  virtual void setControlMessageHandler(ActionHandler* handler)=0;

  /// Set the ActionHandler for remotely-generated control messages.
  virtual void setRemoteControlMessageHandler(ActionHandler* handler)=0;


};

} // namespace plugd

} // namespace flowvr

#endif
