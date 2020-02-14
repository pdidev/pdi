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
* File: src/plugins/flowvr.plugins.UnfilterIt.cpp                 *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// \brief A filter which 'undo' the filtering done by a FilterIt filter.
///
/// <b>Init parameters:</b> none.
///
/// <b>Input ports:</b>
/// -  <b>in</b>: Messages to be filtered.
/// -  <b>stamps</b>: Original message stamps.
/// -  <b>order</b>: Original filtering orders.
///
/// <b>Output Ports:</b>
/// - <b>out</b>: Filtered messages.

class UnfilterIt : public Filter
{
 public:

  UnfilterIt(const std::string &objID);

  virtual ~UnfilterIt();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  enum {
    IDPORT_IN=0,
    IDPORT_STAMPS,
    IDPORT_ORDER,
    NBPORTS
  };

protected:
  virtual void sendPendingOrders(plugd::Dispatcher* dispatcher);

};

using namespace flowvr::xml;

/// Constructor.
UnfilterIt::UnfilterIt(const std::string &objID)
  : Filter(objID)
{
}

UnfilterIt::~UnfilterIt()
{
}

flowvr::plugd::Result UnfilterIt::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  initInputs(NBPORTS);
  inputs[IDPORT_IN]->setName("in");
  inputs[IDPORT_STAMPS]->setName("stamps");
  inputs[IDPORT_ORDER]->setName("order");

  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype = Message::FULL;

  std::cerr << objectID() << " UNFILTER INIT"<<std::endl;
  
  return result;
}

void UnfilterIt::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN && msgnum==0)
  { // Check for special case of iteration -1 message
    int it = 0;
    msg.stamps.read(inputs[IDPORT_IN]->getStampList().it,it);
    if (it==-1)
    {
//      std::cerr << objectID() << " forward msg -1"<<std::endl;
      MessagePut newmsg;
      newmsg.stamps.clone(msg.stamps,&outputs[0]->stamps);
      newmsg.data = msg.data;
      outputs[0]->put(newmsg,dispatcher);
      inputs[IDPORT_IN]->eraseFront();
    }
  }
  sendPendingOrders(dispatcher);
}

void UnfilterIt::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN)
  { // forward specification to out port
    outputs[0]->stamps=inputs[mqid]->getStampList();
    outputs[0]->newStampSpecification(dispatcher);

    sendPendingOrders(dispatcher);
  }
}

void UnfilterIt::sendPendingOrders(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION

  if (!inputs[IDPORT_IN]->stampsReceived()) return; // still waiting for stamps specification

/*
  int it,itin;
  for (;;)
  {
    Message msg;
    int num;
    {
      if (!inputs[IDPORT_ORDER]->frontMsg().valid())
      {
#ifdef DEBUG
	std::cout<<objectID()<<": waiting orders"<<std::endl;
#endif
	return;
      }

      msg = inputs[IDPORT_ORDER]->frontMsg();

      msg.stamps.read(inputs[IDPORT_ORDER]->getStampList().num,num);
      
      msg.stamps.read(inputs[IDPORT_ORDER]->getStampList().it,it);
      bool scratch = (it<0);
      it = (it<0?-it:it)-10;
      itin = -10;
      while(!inputs[IDPORT_STAMPS]->empty()
	    && inputs[IDPORT_STAMPS]->frontMsg().valid()
	    && inputs[IDPORT_STAMPS]->frontMsg().stamps.read(inputs[IDPORT_IN]->getStampList().it,itin)
	    && itin<it)
      {
        inputs[IDPORT_IN]->eraseFront();
      }

      if (scratch)
      {
	inputs[IDPORT_ORDER]->eraseFront();
	continue; // do not send any message, just clean the queue
      }
      if (itin>=it)
        msg = inputs[IDPORT_IN]->frontMsg();
      else
      {
#ifdef DEBUG
	std::cout<<objectID()<<": waiting message "<<it<<std::endl;
#endif
        return; // missing message;
      }
      inputs[IDPORT_ORDER]->eraseFront();
      num = numout++;
    }
#ifdef DEBUG
    std::cout<<objectID()<<": sending message "<<num<<std::endl;
#endif
    MessagePut newmsg;
    newmsg.stamps.clone(msg.stamps,&inputs[IDPORT_IN]->getStampList());
    newmsg.data=msg.data;

    traceout.write(it);
    outputs[0]->put(newmsg,dispatcher,num);
  }
*/

  while (inputs[IDPORT_STAMPS]->frontMsg().valid() && inputs[IDPORT_ORDER]->frontMsg().valid())
  {
    Message msgstamps = inputs[IDPORT_STAMPS]->frontMsg();
    Message msgorder = inputs[IDPORT_ORDER]->frontMsg();
    int itstamps, itorder;

    msgstamps.stamps.read(inputs[IDPORT_STAMPS]->getStampList().it,itstamps);
    msgorder.stamps.read(inputs[IDPORT_ORDER]->getStampList().it,itorder);

    bool scratch = (itorder<0);
    itorder = (itorder<0?-itorder:itorder)-10;

    MessagePut newmsg;

    if (itstamps<itorder)
    { // This message was scratched
      //std::cerr << objectID() << " empty msg "<<itstamps<<std::endl;
      newmsg.data = alloc(0);
      inputs[IDPORT_STAMPS]->eraseFront();
    }
    else if (scratch)
    {
      //std::cerr << objectID() << " scratching up to msg "<<itstamps<<std::endl;
      inputs[IDPORT_ORDER]->eraseFront();
      continue;      
    }
    else if (itstamps>itorder)
    { // This is a duplicate message
      //std::cerr << objectID() << " erase msg "<<itstamps<<std::endl;
      inputs[IDPORT_ORDER]->eraseFront();
      inputs[IDPORT_IN]->eraseFront();
      continue;
    }
    else
    {
      // Wait for corresponding result
      if (!inputs[IDPORT_IN]->frontMsg().valid())
      {
	//std::cerr << objectID() << " waiting msg "<<itstamps<<std::endl;
	break;
      }
      //std::cerr << objectID() << " forward msg "<<itstamps<<std::endl;
      Message msgin = inputs[IDPORT_IN]->frontMsg();
      newmsg.stamps.clone(msgin.stamps,&outputs[0]->stamps);
      newmsg.data = msgin.data;
      inputs[IDPORT_ORDER]->eraseFront();
      inputs[IDPORT_IN]->eraseFront();
      inputs[IDPORT_STAMPS]->eraseFront();      
    }
    newmsg.stamps.write(outputs[0]->stamps.it, itstamps);
    outputs[0]->put(newmsg,dispatcher);
  }

}

flowvr::plugd::GenClass<UnfilterIt> UnfilterItClass("flowvr.plugins.UnfilterIt", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* UnfilterIt::getClass() const
{
  return &UnfilterItClass;
}

} // namespace plugins

} // namespace flowvr
