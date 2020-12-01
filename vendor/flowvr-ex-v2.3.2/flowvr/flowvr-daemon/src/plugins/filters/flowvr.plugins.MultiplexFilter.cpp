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
* File: src/plugins/flowvr.plugins.MultiplexFilter.cpp            *
*                                                                 *
* Contacts:                                                       *
*  06/17/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// MultiplexFilter.
///
/// A filter which re-groups several message streams into one.
/// Remark : the messages are sent in the same order that they are received.
///
/// It changes the it and the num stamps to "hide" the use of multiple streams.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of inputs\</nb\>
///
/// <b>Input ports:</b>
/// -  <b>in<i>#</i></b> with <i>#</i> from 0 to <i>nb</i>-1
///
/// <b>Output Ports:</b>
/// -  <b>out</b>
///
class MultiplexFilter : public Filter
{
 public:

  MultiplexFilter(const std::string &objID);

  virtual ~MultiplexFilter();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

protected:

  bool stampsOk;
  int it;
};

using namespace flowvr::xml;

/// Constructor.
MultiplexFilter::MultiplexFilter(const std::string &objID)
  : Filter(objID)
{
}

MultiplexFilter::~MultiplexFilter()
{
}

flowvr::plugd::Result MultiplexFilter::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()<1) return Result(flowvr::plugd::Result::ERROR,"No nb parameter");
  std::string nb = lnb->item(0)->getTextContent();
  int NBPORTS = atoi(nb.c_str());
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

  it = 0;
  stampsOk = false;

  return result;
}

void MultiplexFilter::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  std::cout << name()<<": new input "<<mqid<<" "<<msgnum<<std::endl;
#endif

  MessagePut m;
  m.data = msg.data;
  m.stamps.clone(msg.stamps,&outputs[0]->stamps);
  m.stamps.write(outputs[0]->stamps.it,it);

  outputs[0]->put(m,dispatcher);

  inputs[mqid]->setFront(msgnum+1);
  ++it;
}

void MultiplexFilter::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (!stampsOk)
  {
    outputs[0]->stamps = inputs[mqid]->getStampList();
    outputs[0]->msgtype = (msg.data.valid()?Message::FULL:Message::STAMPS);
    outputs[0]->newStampSpecification(dispatcher);
    stampsOk = true;
  }
}

flowvr::plugd::GenClass<MultiplexFilter> MultiplexFilterClass("flowvr.plugins.MultiplexFilter", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* MultiplexFilter::getClass() const
{
  return &MultiplexFilterClass;
}

} // namespace plugins

} // namespace flowvr
