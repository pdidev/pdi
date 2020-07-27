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
* File: src/plugins/flowvr.plugins.FilterIt.cpp                   *
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

/// \brief Transmits messages from 'in' to 'out'. Transmits only
/// messages that have the same it as the current messages on the
/// 'order' port. Older messages are discarded. Newer ones wait in the
/// queue. Also does some black magic when order's it is negative.
///
/// <b>Init parameters:</b> none.
///
/// <b>Input ports:</b>
/// -  <b>in</b>: Messages to be filtered.
/// -  <b>order</b>: Stamps, filtering orders (from a synchronizer such as flowvr::plugins::GreedySynchronizor).
///
/// <b>Output Ports:</b>
/// - <b>out</b>: Filtered messages.
/// - <b>stat</b>: Statistic about filtering

class FilterIt : public Filter
{
 public:

  FilterIt(const std::string objID);

  virtual ~FilterIt();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  enum {
    IDPORT_IN=0,
    IDPORT_ORDER,
    NBPORTS
  };

  int numout;

protected:
  virtual void sendPendingOrders(plugd::Dispatcher* dispatcher);

  TypedTrace<int> traceout;

};

using namespace flowvr::xml;

/// Constructor.
FilterIt::FilterIt(const std::string objID)
  : Filter(objID), numout(0), traceout(TypedTrace<int>("out"))
{
}

FilterIt::~FilterIt()
{
}

flowvr::plugd::Result FilterIt::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  //initialization of the input message queue
  initInputs(NBPORTS);
  inputs[IDPORT_IN]->setName("in");
  inputs[IDPORT_ORDER]->setName("order");


  //initialization of the OMQ "out"
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype = Message::FULL;

  outputTraces.push_back(&traceout);

  return result;
}


void FilterIt::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  if (mqid == IDPORT_IN)
    std::cout << objectID()<<": new input "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
  else
    std::cout << objectID()<<": new order "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
  sendPendingOrders(dispatcher);
}

void FilterIt::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN)
  { // forward specification to out port
#ifdef DEBUG
    std::cout << objectID()<<": forwarding stamps specification"<<std::endl;
#endif

    //forwarding stamplist specification to the OMQ
    outputs[0]->stamps=inputs[mqid]->getStampList();
    outputs[0]->newStampSpecification(dispatcher);

    sendPendingOrders(dispatcher);
  }
}

void FilterIt::sendPendingOrders(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION

  if (!inputs[IDPORT_IN]->stampsReceived()) return; // still waiting for stamps specification

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
      //std::cout<<"Attente du message :"<<it<<std::endl;
      //if(inputs[IDPORT_IN]->empty()) std::cout<<"La file de message est vide"<<std::endl;
      //if(!inputs[IDPORT_IN]->frontMsg().valid()) std::cout<<"Le message est invalide"<<std::endl;
      //if(!inputs[IDPORT_IN]->frontMsg().stamps.read(inputs[IDPORT_IN]->getStampList().it,itin)) std::cout<<"La lecture du stamp a echouee"<<std::endl;
      //if(!(itin<it)) std::cout<<"Le numero d'iteration n'est pas bon ("<<itin<<">="<<it<<")"<<std::endl;
      while(!inputs[IDPORT_IN]->empty()
	    && inputs[IDPORT_IN]->frontMsg().valid()
	    && inputs[IDPORT_IN]->frontMsg().stamps.read(inputs[IDPORT_IN]->getStampList().it,itin)
	    && itin<it){
        inputs[IDPORT_IN]->eraseFront();
      }

      if (scratch)
      {
#ifdef DEBUG
	std::cout<<objectID()<<": scratch order message"<<it<<std::endl;
#endif
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
        return;
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
}

flowvr::plugd::GenClass<FilterIt> FilterItClass("flowvr.plugins.FilterIt", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* FilterIt::getClass() const
{
  return &FilterItClass;
}

} // namespace plugins

} // namespace flowvr
