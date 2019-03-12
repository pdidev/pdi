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
* File: include/flowvr/plugins/regulator.h                        *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGINS_REGULATOR_H
#define FLOWVR_PLUGINS_REGULATOR_H

#include "flowvr/daemon.h"
#include "flowvr/plugd/genclass.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/ipc/mtsignal.h"
#include "flowvr/plugins/baseobject.h"
#include <flowvr/thread.h>

#include <vector>

namespace flowvr
{

namespace plugins
{

class MessageQueueSignal;
class Dispatcher;

class Regulator : public BaseObject,
                  public flowvr::Thread
{
public:
  /// Constructor.
  Regulator(const std::string &objID);
  ~Regulator();

  MPModuleDescription *getModuleDescription() const { return header; }
  class MPPortQueue
  {
  public:
    MPPort* port;
    std::string stampsSpecif;
  };

  MPPortQueue *getPorts() const { return ports; }
private:
  BufferWrite                headerBuf;
  MPModuleDescription*       header;
  flowvr::plugd::Dispatcher* threadDispatcher;
  bool                       closed;

  int              nbMessageToGo,
                   nbEventPorts; ///< number of messages needed to end the wait
  std::vector<int> waitOnInput; ///< true if waiting for a message on port i

  ipc::MTSignal endWaitSignal; ///< signal the end of the wait operation. Also signal the start of the module


  MPPortQueue* ports;
  std::vector<MPPortQueue*> allPorts;

  void endWaitOnInput(int mqid, const Message& msg);

public:
  //ipc::MTLock& messageQueueLock(int mqid) { return statusLock; }
  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, plugd::Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, plugd::Dispatcher* dispatcher);



  virtual plugd::Class* getClass() const;

  /// Initialization. Returns a XML document containing the result.
  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher);

  static flowvr::xml::DOMNode* buildPortDescription(const MPPort* p);

  static flowvr::xml::DOMNode* moduleXmlDescription(const MPModuleDescription* header);

  virtual flowvr::xml::DOMNode* getXmlDescription();

  virtual void processCmdClose(flowvr::MPModuleDescription::Cmd& cmd);

  virtual void processCmdInvalid(flowvr::MPModuleDescription::Cmd& cmd);

  virtual void processCmdWait(flowvr::MPModuleDescription::Cmd& cmd);

  virtual void processCmdPut(flowvr::MPModuleDescription::Cmd& cmd);
  
  virtual void processCmdStamp(flowvr::MPModuleDescription::Cmd& cmd);
  
  virtual void processCmdCpuset(flowvr::MPModuleDescription::Cmd& cmd);

  virtual void processReplyMessage(const Message& msg, const std::string& port, plugd::Dispatcher* dispatcher);

  virtual flowvr::plugd::Result close(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher);

  /// Main thread function. Should be implemented by subclasses.
  virtual int run();

  /// Execute an action in immediate mode.
  virtual flowvr::plugd::Result doAction(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher);

  /// Create a one-line description of the current state
  virtual std::string status() const;

protected:
  virtual void doStart(plugd::Dispatcher* dispatcher);

  TypedTrace<int> traceBeginIt;
  TypedTrace<int> traceEndIt;

  mutable double lastStatusTime; ///< last status update time to compute iterations per second
  mutable double lastStatusIt; ///< last status update iteration to compute iterations per second

  /// Create an ActionHandler for batch mode action execution.
  //virtual flowvr::plugd::ActionHandler* createAction(flowvr::xml::DOMElement* xmlRoot);

};

extern flowvr::plugd::GenClass<Regulator> RegulatorClass;

} // namespace plugins

} // namespace flowvr

#endif
