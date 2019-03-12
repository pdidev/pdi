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
* File: src/plugins/flowvr.plugins.PreSignal.cpp                  *
*                                                                 *
* Contacts:                                                       *
*  06/21/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// PreSignal.
///
/// Send nb empty messages at startup, then transmit incomming messages.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of messages to send\</nb\> (default 1)
///
/// <b>Input ports:</b>
/// -  <b>in</b>
///
/// <b>Output Ports:</b>
/// - <b>out</b>

class PreSignal : public Filter
{
 public:

  PreSignal(const std::string &objID);

  virtual ~PreSignal();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newStampListSpecification(int mqid, const Message& msg, plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

  int nbPreMessage;
};

using namespace flowvr::xml;

/// Constructor.
PreSignal::PreSignal(const std::string &objID)
  : Filter(objID), nbPreMessage(1)
{
}

PreSignal::~PreSignal()
{
}

flowvr::plugd::Result PreSignal::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()>0)
  {
    std::string nb = lnb->item(0)->getTextContent();
    nbPreMessage = atoi(nb.c_str());
  }
  delete lnb;

  initInputs(1);
  inputs[0]->setName("in");

  initOutputs(1);
  outputs[0]->setName("out");

  return result;
}

void PreSignal::newStampListSpecification(int mqid, const Message& msg, plugd::Dispatcher* dispatcher)
{
  if (mqid==0)
  {
    outputs[0]->stamps = inputs[0]->getStampList();
    outputs[0]->msgtype = (msg.data.valid()?Message::FULL:Message::STAMPS);
    outputs[0]->newStampSpecification(dispatcher);

    // send the N first messages

    for (int i=0;i<nbPreMessage;i++)
    {
      MessageWrite m;
      m.stamps.write(outputs[0]->stamps.it,-1);
      if (outputs[0]->msgtype == Message::FULL) m.data = alloc(0);
      outputs[0]->put(m,dispatcher,i);
    }

  }
}

void PreSignal::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  std::cout << name()<<": new input "<<mqid<<" "<<msgnum<<std::endl;
#endif

  MessagePut m;
  m.data = msg.data;
  m.stamps.clone(msg.stamps,&outputs[0]->stamps);
  outputs[0]->put(m,dispatcher,msgnum+nbPreMessage);

  inputs[mqid]->setFront(msgnum+1);
}

flowvr::plugd::GenClass<PreSignal> PreSignalClass("flowvr.plugins.PreSignal", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* PreSignal::getClass() const
{
  return &PreSignalClass;
}

} // namespace plugins

} // namespace flowvr
