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
* File: src/plugins/flowvr.plugins.MaxBuffer.cpp                  *
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

/// MaxBuffer.
///
/// Read the endIt signals and the it of the messages read by a module and send the it + the max number of allowed buffers
class MaxBuffer : public Filter
{
 public:

  MaxBuffer(const std::string &objID);

  virtual ~MaxBuffer();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

  enum {
    IDPORT_PORTIT=0,
    IDPORT_SIGNAL,
    NBPORTS
  };

protected:
  //StampList stamps;
  int maxBuffers;

  int numout;
  //string sourceout;

  virtual void doStart(plugd::Dispatcher* dispatcher);
  virtual void sendPendingOrders(plugd::Dispatcher* dispatcher, bool start=false);
};

using namespace flowvr::xml;

/// Constructor.
MaxBuffer::MaxBuffer(const std::string &objID)
  : Filter(objID)
{
  //sourceout = objID+":out";
  numout = 0;
  maxBuffers=0;
}

MaxBuffer::~MaxBuffer()
{
}

flowvr::plugd::Result MaxBuffer::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  maxBuffers=1;
  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()<1) return Result(flowvr::plugd::Result::ERROR,"No nb parameter");
  std::string nb = lnb->item(0)->getTextContent();
  maxBuffers = atoi(nb.c_str());
  delete lnb;

  initInputs(NBPORTS);
  inputs[IDPORT_PORTIT]->setName("portIt");
  inputs[IDPORT_SIGNAL]->setName("signal");

  //this filter has just one output port
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype=Message::STAMPS;
  outputs[0]->newStampSpecification(dispatcher);

  return result;
}

void MaxBuffer::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  if (mqid == IDPORT_PORTIT)
    std::cout << name()<<": new input "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
  else
    std::cout << name()<<": new signal "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
  if (isStarted())
    sendPendingOrders(dispatcher);
}

void MaxBuffer::doStart(plugd::Dispatcher* dispatcher)
{
  Filter::doStart(dispatcher);
  //StampList stamps;
  { // out
    //StampListSpecification stampsSpec;
    //MessageWrite msgspec;
    //msgspec.stamps.write(stampsSpec.source,sourceout);
    //msgspec.stamps.write(stampsSpec.num,-1);
    //msgspec.stamps.write(stampsSpec.it,0);
    //xml::DOMNode* xmlspec = stamps.generateXML();
    //msgspec.stamps.write(stampsSpec.spec,xml::DOMWriter::toString(xmlspec));
    //delete xmlspec;
    //dispatcher->process(msgspec);
  }
  sendPendingOrders(dispatcher,true);
}

void MaxBuffer::sendPendingOrders(plugd::Dispatcher* dispatcher, bool start)
{ // MAIN FILTER FUNCTION
  int it = -1;
  int num;
  {
    //ipc::ScopedMTLock locker(globalLock,"sendPendingOrders");
    int modit = inputs[IDPORT_SIGNAL]->backNum();
    inputs[IDPORT_SIGNAL]->setFront(modit); // erase all previous signal messages
    for (num=inputs[IDPORT_PORTIT]->frontNum();num<=modit;num++)
    {
      if (inputs[IDPORT_PORTIT]->get(num).valid())
      { // we found a new message
	inputs[IDPORT_PORTIT]->get(num).stamps.read(inputs[IDPORT_PORTIT]->getStampList().it,it);
	inputs[IDPORT_PORTIT]->setFront(num+1); // erase this message as we read it
      }
    }
    if (it>=0)
    { // we removed a new message
      num=numout;
      ++numout;
    }
  }
  if (it>-1 || start)
  {
#ifdef DEBUG
    std::cout<<name()<<": sending message "<<num<<" it="<<it<<std::endl;
#endif
    //MessageWrite m;
    //m.stamps.write(stamps.source,sourceout);
    //m.stamps.write(stamps.num,num);
    //m.stamps.write(stamps.it,it+maxBuffers);
    //dispatcher->process(m);

    MessageWrite m;
    m.stamps.write(outputs[0]->stamps.it,it+maxBuffers);
    outputs[0]->put(m,dispatcher,num);

  }
}

flowvr::plugd::GenClass<MaxBuffer> MaxBufferClass("flowvr.plugins.MaxBuffer", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* MaxBuffer::getClass() const
{
  return &MaxBufferClass;
}

} // namespace plugins

} // namespace flowvr
