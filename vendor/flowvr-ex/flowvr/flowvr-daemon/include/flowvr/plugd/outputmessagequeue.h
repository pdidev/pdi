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
* File: include/flowvr/plugd/messagequeue.h                       *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGD_OUTPUTMESSAGEQUEUE_H
#define FLOWVR_PLUGD_OUTPUTMESSAGEQUEUE_H

#include "flowvr/message.h"
#include "flowvr/ipc/atomic.h"
#include "flowvr/ipc/mtlock.h"
#include "flowvr/xml.h"
#include "flowvr/stamp.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/routingtable.h"
#include "flowvr/trace.h"
#include <deque>
#include <vector>

namespace flowvr
{

namespace plugd
{

  /// OutputMessageQueue, object linked to each output of any object (filter, regulator and synchronizor).
  /// This object process the messages to the dispatcher with the list of actions associated.
  /// OutputMessageQueue is also useful to buffer messages in order to pause the application, a boolean variable allows to make it active or not.

class OutputMessageQueue
{
 protected:

  typedef std::deque<Message> Queue;

  /// Message buffer
  Queue buffer;

  /// name of the objet containing this outputmessagequeue
  std::string objectID;

  /// name of the source
  std::string source;
  
  /// name of the outputmessagequeue
  std::string name;

  /// the action vector associated in the routing table to this source
  const std::vector<Action*>* actionvector;

  /// number of message send by this output message queue
  ipc::MPAtomicInt count;

  /// to set the num of the message (can be different with the count value)
   ipc::MPAtomicInt numout;

  /// allow to control the activity of the output message queue
  bool active;

  /// to test if the stamplist specification is valid
  bool valid;

  ipc::MTLock locker;

 public:

  /// trace for the output message queue
  TypedTrace<int> trace;

  /// Stamps specification
  StampList stamps;

  Message::Type msgtype; 
 
  OutputMessageQueue(const std::string &ObjectID);

  const std::string& getSource() const
  {
    return source;
  }
  
  /// get the name of the outputmessage queue
  const std::string& getName() const
  {
    return name;
  }

  /// get the num value for the next message
  int getNextNum() const
  {
    return numout;
  }

  /// to set the source of the outputmessagequeue based on the object id and the name of the associated port
  void setName(const std::string &portname)
    {
      if (active)
	{
	  std::cerr<<"OutputMessageQueue::setName cannot be called after start() for "<<(objectID+":"+portname)<<std::endl;
	  exit(1);
	}
      source=objectID+":"+portname;
      trace.setName(portname);
      name=portname;
    }

  /// Execute the action processing the specified message.
  /// The dispatcher parameter is used to dispatch any generated message
  void put(MessagePut  msg, Dispatcher* dispatcher, int num=-1);

  /// to stop the dataflow in order to pause a flowvr application
  int pause();

  /// used when the application is lauching or relaunching (no difference is made between a launch of a new application and a pause/restart)
  void start(Dispatcher* dispatcher,int it=0);

  void newStampSpecification(Dispatcher* dispatcher, int it=0);

  /// return true if no action is linked to this outputmessagequeue
  bool isConnected()
  {
    return (actionvector!=NULL && !actionvector->empty());
  }

protected:
  /// Called once start and newStampSpecification are done.
  void go(Dispatcher* dispatcher);

};

} // namespace plugins

} // namespace flowvr

#endif
