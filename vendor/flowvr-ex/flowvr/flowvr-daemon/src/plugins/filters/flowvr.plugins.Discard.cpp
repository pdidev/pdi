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
* File: ./src/plugins/filters/flowvr.plugins.Discard.cpp          *
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

/// Transmits input messages, unless a message starting with '\0' is
/// received on port 'open'.
///
/// <b>Input ports:</b>
/// -  <b>in</b>
/// -  <b>open</b>
///
/// <b>Output Ports:</b>
/// - <b>out</b>
/// @ingroup Filters
class Discard : public Filter
{


public:

  bool open;

  /// Constructor.
  Discard(const std::string &objID)
  : Filter(objID), open(true)
  {}

  virtual Class* getClass() const;

  virtual Result init(flowvr::xml::DOMElement* xmlRoot, Dispatcher* dispatcher)
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

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
  {
    switch (mqid)
    {
    case 0:
    {
      if (open)
      {
	MessagePut mput;
	mput.stamps.clone(msg.stamps,&outputs[0]->stamps);
	mput.data = msg.data;
	outputs[0]->put(mput,dispatcher);
      }
      break;
    }
    case 1:
    {
      open = (msg.data.getSize()>=1 && *msg.data.getRead<unsigned char>()!=0);
      break;
    }
    }
    inputs[mqid]->setFront(msgnum+1);
  }

  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
  {
    if (mqid==0)
    {
      outputs[0]->stamps = inputs[0]->getStampList();
      outputs[0]->msgtype = (msg.data.valid()?Message::FULL:Message::STAMPS);
      outputs[0]->newStampSpecification(dispatcher);
    }
  }

};

GenClass<Discard> DiscardClass("flowvr.plugins.Discard", // name
					"" // description
);

Class* Discard::getClass() const
{
  return &DiscardClass;
}

} // namespace plugins

} // namespace flowvr
