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
* File: src/plugins/flowvr.plugins.It2Start.cpp                   *
*                                                                 *
* Contacts:                                                       *
*  01/16/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugins/filter.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include <iostream>
#include <sstream>
#include <unistd.h>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

/// It2Start.
///
/// renumber input messages (unclear how). 
class It2Start : public Filter
{
 public:

  It2Start(const std::string &objID);

  virtual ~It2Start();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

  enum {
    IDPORT_IT=0,
    NBPORTS
  };

protected:
  //StampList stamps;

  int numout;
  //string sourceout;

  virtual void doStart(plugd::Dispatcher* dispatcher);
  virtual void sendPendingOrders(plugd::Dispatcher* dispatcher, bool start=false);
};

using namespace flowvr::xml;

/// Constructor.
It2Start::It2Start(const std::string &objID)
  : Filter(objID)
{
  //sourceout = objID+":out";
  numout = 0;
}

It2Start::~It2Start()
{
}

flowvr::plugd::Result It2Start::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  //init input message queue
  initInputs(NBPORTS);
  inputs[IDPORT_IT]->setName("it");

  //init output message queue
  initOutputs(NBPORTS);
  outputs[IDPORT_IT]->setName("out");
  outputs[IDPORT_IT]->msgtype=Message::STAMPS;
  outputs[IDPORT_IT]->newStampSpecification(dispatcher);
  
  return result;
}

void It2Start::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
  if (isStarted())
    sendPendingOrders(dispatcher);
}

void It2Start::doStart(plugd::Dispatcher* dispatcher)
{
  Filter::doStart(dispatcher);
  //StampList stamps;
  //{ // out
  //StampListSpecification stampsSpec;
  //MessageWrite msgspec;
  //msgspec.stamps.write(stampsSpec.source,sourceout);
  //msgspec.stamps.write(stampsSpec.num,-1);
  //msgspec.stamps.write(stampsSpec.it,0);
  //xml::DOMNode* xmlspec = stamps.generateXML();
  //msgspec.stamps.write(stampsSpec.spec,xml::DOMWriter::toString(xmlspec));
  //delete xmlspec;
  //dispatcher->process(msgspec);
  //}
  sendPendingOrders(dispatcher,true);
}

void It2Start::sendPendingOrders(plugd::Dispatcher* dispatcher, bool start)
{ // MAIN FILTER FUNCTION
  int it = 0;
  int num0;
  {
    //ipc::ScopedMTLock locker(globalLock,"sendPendingOrders");
    if (inputs[IDPORT_IT]->backMsg().valid())
    {
      inputs[IDPORT_IT]->backMsg().stamps.read(inputs[IDPORT_IT]->getStampList().it,it);
      inputs[IDPORT_IT]->setFront(inputs[IDPORT_IT]->backNum()+1); // erase this message as we read it
    }
    num0=numout;
    if (it>=numout)
    { // we removed a new message
      numout=it+1;
    }
  }
#ifdef DEBUG
  std::cout<<name()<<": sending message(s) "<<num0<<" to "<<it<<std::endl;
#endif
  while (num0<=it)
  {
    MessageWrite m;
    //m.stamps.write(stamps.source,sourceout);
    //m.stamps.write(stamps.num,num0);
    m.stamps.write(outputs[IDPORT_IT]->stamps.it,num0);
 
    //dispatcher->process(m);
    outputs[IDPORT_IT]->put(m,dispatcher,num0);
    ++num0;
  }
}

flowvr::plugd::GenClass<It2Start> It2StartClass("flowvr.plugins.It2Start", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* It2Start::getClass() const
{
  return &It2StartClass;
}

} // namespace plugins

} // namespace flowvr
