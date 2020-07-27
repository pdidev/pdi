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
* File: src/plugins/flowvr.plugins.MaskRouting.cpp                *
*                                                                 *
* Contacts:                                                       *
*  12/02/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// \brief A filter which route a message in N streams according to
/// a mask stamps dynamically specifying where to send each message.
///
/// This  filter opens  one input  port <b>in</b>  and  several output
/// ports <b>out0,out1,...</b>.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of outputs to produce\</nb\>
///
/// <b>Input ports:</b>
/// - <b>in</b>
///
/// <b>Output Ports:</b>
/// -  <b>out<i>#</i></b> with <i>#</i> from 0 to <i>nb</i>-1

class MaskRouting : public Filter
{
public:

  int nbPorts;
  int maskSize;
  StampInfo* stampmask;
  int* mask;

  enum {
    IDPORT_IN=0,
    NBPORTS
  };

  MaskRouting(std::string objID);

  virtual ~MaskRouting();

  virtual flowvr::plugd::Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);
  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

};

MaskRouting::MaskRouting(std::string objID)
: Filter(objID), nbPorts(0), maskSize(0), stampmask(NULL), mask(NULL)
{
}

MaskRouting::~MaskRouting()
{
}

flowvr::plugd::Result MaskRouting::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot,dispatcher);
  if (result.error()) return result;

  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()<1) return flowvr::plugd::Result(flowvr::plugd::Result::ERROR,"No nb parameter");
  std::string nb = lnb->item(0)->getTextContent();
  nbPorts = atoi(nb.c_str());
  delete lnb;

  maskSize = nbPorts / 32;
  mask = new int[maskSize];

  initInputs(1);
  inputs[IDPORT_IN]->setName("in");

  initOutputs(nbPorts);
  
  char buf[16];
  for(int i = 0; i < nbPorts; i++)
  {
    sprintf(buf,"out%d",i);
    outputs[i]->setName(buf);
    outputs[i]->msgtype=Message::FULL;
  }


  return result;
}

void MaskRouting::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  std::cout << name()<<": new input, port id"<<mqid<<", queue size "<<inputs[mqid]->size()<<", message num "<<msgnum<<std::endl;
#endif

  while ( inputs[IDPORT_IN]->frontMsg().valid() )
  {
    const Message& msg = inputs[IDPORT_IN]->frontMsg();

    if (stampmask!=NULL)
    {
      for (int i=0; i < maskSize; i++)
	msg.stamps.read((*stampmask)[i],mask[i]);
    }
    else
      for (int i=0; i < maskSize; i++)
	mask[i]=-1;

    for(int i = 0; i < nbPorts; i++)
    {
      if (mask[i>>5]&(1<<(i&31)))
      {
	MessagePut newmsg;
	newmsg.data = msg.data;
	newmsg.stamps.clone(msg.stamps,&inputs[IDPORT_IN]->getStampList());

	outputs[i]->put(newmsg, dispatcher);
      }
    }
    inputs[IDPORT_IN]->eraseFront();
  }
}

void MaskRouting::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN)
  { // forward specification to out port
    stampmask = inputs[IDPORT_IN]->getStampList()["mask"];
    for(int i = 0; i < nbPorts; i++)
    {
      outputs[i]->stamps = inputs[IDPORT_IN]->getStampList();
      outputs[i]->newStampSpecification(dispatcher);
    }
  }
}

flowvr::plugd::GenClass<MaskRouting> MaskRoutingClass("flowvr.plugins.MaskRouting", // name
					      "" // description
					      );

Class* MaskRouting::getClass() const
{
  return &MaskRoutingClass;
}

} // namespace plugd

} // namespace flowvr
