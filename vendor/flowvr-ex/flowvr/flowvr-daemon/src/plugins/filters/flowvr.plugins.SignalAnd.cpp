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
* File: src/plugins/flowvr.plugins.SignalAnd.cpp                  *
*                                                                 *
* Contacts:                                                       *
*  01/16/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// SignalAnd.
///
/// Do an AND on N signals (i.e. wait for one message on N ports and send one message).
///
/// This  filter  opens  several  input ports  <b>in0,in1,...</b>  and
/// produces  messages  in an  output  port  <b>out</b>.   When a  new
/// message  is  available  on  all  input  ports  a  new  message  is
/// constructed by taking  the stamps of  the first  message.
/// This message is then sent to the <b>out</b> port.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of inputs\</nb\>
///
/// <b>Input ports:</b>
/// -  <b>in<i>#</i></b> with <i>#</i> from 0 to <i>nb</i>-1
///
/// <b>Output Ports:</b>
/// - <b>out</b>

class SignalAnd : public Filter
{
 public:

  SignalAnd(const std::string & objID);

  virtual ~SignalAnd();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  int NBPORTS;

protected:
  BufferPool poolout;

  virtual void sendPendingMessages(plugd::Dispatcher* dispatcher);
};

using namespace flowvr::xml;

/// Constructor.
SignalAnd::SignalAnd(const std::string &objID)
: Filter(objID), NBPORTS(0)
{
}

SignalAnd::~SignalAnd()
{
}

flowvr::plugd::Result SignalAnd::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()<1) return Result(flowvr::plugd::Result::ERROR,"No nb parameter");
  std::string nb = lnb->item(0)->getTextContent();
  NBPORTS = atoi(nb.c_str());
  delete lnb;

  initInputs(NBPORTS);
  char buf[16];
  for (int i=0;i<NBPORTS;i++)
  {
    sprintf(buf,"in%d",i);
    inputs[i]->setName(buf);
  }
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype = Message::STAMPS;
//  outputs[0]->newStampSpecification(dispatcher);

  return result;
}

void SignalAnd::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  std::cout << name()<<": new input "<<mqid<<" "<<msgnum<<std::endl;
#endif
  sendPendingMessages(dispatcher);
}

void SignalAnd::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == 0)
  { // forward specification to out port
    //give the Stamplist to the outputmessage queue    
    outputs[0]->stamps  = inputs[0]->getStampList();
//    outputs[0]->msgtype = (msg.data.valid()?Message::FULL:Message::STAMPS);
    outputs[0]->newStampSpecification(dispatcher);

  }
  sendPendingMessages(dispatcher);
}

void SignalAnd::sendPendingMessages(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION
  int p;
  for(;;)
  {
    for (p=0; p<NBPORTS; p++)
      if (!inputs[p]->frontMsg().valid())
        break; // next message not ready on port p
    if ( p<NBPORTS )
      break; // at least one missing message/signal

    // we have all messages ready

    size_t size = inputs[0]->frontMsg().data.getSize();
    for (p=1;p<NBPORTS;p++)
    {
      size_t s = inputs[p]->frontMsg().data.getSize();
      if (s<size) size=s;
    }

    MessagePut m;

    m.stamps.clone(inputs[0]->frontMsg().stamps,&inputs[0]->getStampList()); // get stamps from first message
/*
    if (size==0) m.data = inputs[0]->frontMsg().data;
    else
    {
      BufferWrite data = poolout.alloc(getAllocator(),size);
      
      for ( p=0; p<NBPORTS; p++)
      {
	const Message& mread = inputs[p]->frontMsg();
	if (p==0) memcpy(data.writeAccess(), mread.data.readAccess(), size);
	else
	{
	  for (int i=0;i<size;i++)
	    *data.getWrite<char>(i) &= *mread.data.getRead<char>(i);
	}
      }

      m.data = data;
    }
*/
    for ( p=0; p<NBPORTS; p++)
      inputs[p]->eraseFront();

    // send the message
    outputs[0]->put(m,dispatcher);

  }
}

flowvr::plugd::GenClass<SignalAnd> SignalAndClass("flowvr.plugins.SignalAnd", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* SignalAnd::getClass() const
{
  return &SignalAndClass;
}

} // namespace plugins

} // namespace flowvr
