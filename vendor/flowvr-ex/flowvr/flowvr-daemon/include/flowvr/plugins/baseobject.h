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
* File: include/flowvr/plugins/baseobject.h                       *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGINS_BASEOBJECT
#define FLOWVR_PLUGINS_BASEOBJECT

#include "flowvr/plugd/object.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/plugd/outputmessagequeue.h"

namespace flowvr
{

namespace plugins
{

/// Port-based (filters, synchronizors) Objects base class.
class BaseObject : public plugd::Object
{
 public:

  /// Constructor
  BaseObject(const std::string &objID);

  /// Initialization. Returns a XML document containing the result.
  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  /// Execute an action in immediate mode.
  virtual flowvr::plugd::Result doAction(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  /// Create an ActionHandler for batch mode action execution.
  virtual flowvr::plugd::ActionHandler* createAction(flowvr::xml::DOMElement* xmlRoot);

  /// Create a one-line description of the current state
  virtual std::string status() const;

 protected:

  virtual ~BaseObject();

  virtual void doStart(plugd::Dispatcher* dispatcher) {}

  typedef plugd::MessageQueue<BaseObject, ipc::ScopedMTLock> MyMessageQueue;

  virtual void initInputs(int nb);
  virtual int  addInputs(int nb);

  //modification by Lo�ck
  //adding outputmessagequeue to base object
  virtual void initOutputs(int nb);
  virtual int  addOutputs(int nb);

 private:
  mutable ipc::MTLock globalLock; ///< Global Lock (message queues, internal state)

private:
  bool started; ///< Is this object started?
public:

  bool isStarted() const;
  
  int nbInputs;
  int nbOutputs;

  MyMessageQueue            **inputs;
  plugd::OutputMessageQueue **outputs;
  

  /**
   * @todo note that the mqid is ignored and ALL queues are locked.
   *       Currently we can use this side-effect, but beware of a change
   * @param mqid get the lock for message queue mqid (currently IGNORED)
   * @return a reference to the lock (non-const, but can be used in const-contexts)
   */
  ipc::MTLock& messageQueueLock(int mqid=-1) const { return globalLock; }
  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, plugd::Dispatcher* dispatcher)=0;
  virtual void newStampListSpecification(int mqid, const Message& msg, plugd::Dispatcher* dispatcher) {}

};

} // namespace plugins

} // namespace flowvr

#endif
