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
* File: ./src/plugins/filters/flowvr.plugins.Stop.cpp             *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugd/genclass.h"
#include "flowvr/plugins/filter.h"
#include <iostream>
#include <sstream>
#include <unistd.h>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

/// Transmit messages, until a message starting with '\0' arrives on
/// port 'open'. Then discard messages.
///
/// <b>Input ports:</b>
/// -  <b>in</b>
/// -  <b>open</b>
///
/// <b>Output Ports:</b>
/// - <b>out</b>
class Stop : public Filter
{
public:

  bool open;

  /// Constructor.
  Stop(const std::string &objID)
  : Filter(objID), open(true)
  {}

  virtual flowvr::plugd::Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
  {
    Result result = Object::init(xmlRoot,dispatcher);
    if (result.error()) return result;

    initInputs(2);
    inputs[0]->setName("in");
    inputs[1]->setName("open");

    initOutputs(1);
    outputs[0]->setName("out");

    return result;
  }

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, plugd::Dispatcher* dispatcher)
  {
    if (mqid == 1)
    {
      open = (msg.data.getSize()>=1 && *msg.data.getRead<unsigned char>()!=0);
      inputs[mqid]->setFront(msgnum+1);     
    }

    if (open)
    {
      while (inputs[0]->frontMsg().valid())
      {
	Message m = inputs[0]->frontMsg();
	MessagePut mput;
	mput.stamps.clone(m.stamps,&outputs[0]->stamps);
	mput.data = m.data;
	outputs[0]->put(mput,dispatcher);
	inputs[0]->eraseFront();
      }
    }
  }

  virtual void newStampListSpecification(int mqid, const Message& msg, plugd::Dispatcher* dispatcher)
  {
    if (mqid==0)
    {
      outputs[0]->stamps = inputs[0]->getStampList();
      outputs[0]->msgtype = (msg.data.valid()?Message::FULL:Message::STAMPS);
      outputs[0]->newStampSpecification(dispatcher);
    }
  }

};

flowvr::plugd::GenClass<Stop> StopClass("flowvr.plugins.Stop", // name
					"" // description
);

flowvr::plugd::Class* Stop::getClass() const
{
  return &StopClass;
}

} // namespace plugins

} // namespace flowvr
